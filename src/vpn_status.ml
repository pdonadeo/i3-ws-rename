open Constants

class ['a] modulo instance_name status_pipe color sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [`r | `w]
    inherit ['a] Lwt_module.base_modulo instance_name status_pipe
    val! name = "vpn_status"
    val mutable active_vpn = BatSet.String.empty

    method private is_vpn manager path_str =
      let path = OBus_path.of_string path_str in
      let conn_proxy = OBus_proxy.make ~peer:(Nm_manager.to_peer manager) ~path in

      let vpn_prop =
        OBus_property.make Nm_interfaces.Org_freedesktop_NetworkManager_Connection_Active.p_Vpn conn_proxy
      in

      let p_Id =
        {
          OBus_member.Property.interface = "org.freedesktop.NetworkManager.Connection.Active";
          OBus_member.Property.member = "Id";
          OBus_member.Property.typ = OBus_value.C.basic_string;
          OBus_member.Property.access = OBus_member.Property.readable;
          OBus_member.Property.annotations = [];
        }
      in
      let id_prop = OBus_property.make p_Id conn_proxy in

      let%lwt conn_id = OBus_property.get id_prop in
      let%lwt is_vpn = OBus_property.get vpn_prop in

      Lwt.return (conn_id, is_vpn)

    method private update_active_vpn manager new_list =
      let%lwt new_set =
        Lwt_list.fold_left_s
          (fun set path ->
            let%lwt conn_id, vpn = self#is_vpn manager path in
            if vpn then BatSet.String.add conn_id set |> Lwt.return else Lwt.return set)
          BatSet.String.empty
          new_list
      in

      active_vpn <- new_set;
      Lwt.return ()

    method private network_manager () =
      let open Lwt_react in
      let open Lwt in
      let open OBus_value in
      (* Get the manager. *)
      let%lwt manager = Nm_manager.daemon () in

      let nm_proxy =
        OBus_proxy.make ~peer:(Nm_manager.to_peer manager) ~path:["org"; "freedesktop"; "NetworkManager"]
      in

      let sig_desc = OBus_signal.make Nm_interfaces.Org_freedesktop_NetworkManager.s_PropertiesChanged nm_proxy in

      (* Connects to this signal. *)
      let%lwt event = OBus_signal.connect sig_desc in

      E.keep
        (E.map_s
           (fun properties ->
             Lwt_list.iter_s
               (fun (k, v) ->
                 if k = "ActiveConnections"
                 then
                   let active_now = C.cast_single (C.array C.basic_object_path) v |> List.map OBus_path.to_string in
                   let%lwt () = self#update_active_vpn manager active_now in
                   let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
                   return ()
                 else return ())
               properties)
           event);

      return ()

    method! run () : unit Lwt.t =
      Utils.detach_promise self#network_manager "Vpn_status.modulo running self#network_manager method";
      Lwt.wakeup (snd ready) ();
      fst ready

    method! json () =
      let icon = "" in
      let vpns = BatSet.String.to_list active_vpn |> List.sort String.compare in
      let vpn_icons = List.map (spf "%s: %s" icon) vpns in
      let full_text, short_text = if List.length vpns <> 0 then (String.concat " " vpn_icons, icon) else ("", "") in

      let bl =
        {
          I3bar_protocol.Block.default with
          full_text;
          short_text;
          color;
          name;
          instance = instance_name;
          separator = sep;
        }
      in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
  end
