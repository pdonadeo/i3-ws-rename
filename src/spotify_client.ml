open Spotify_interfaces


module Org_freedesktop_DBus_Introspectable =
struct
  open Org_freedesktop_DBus_Introspectable


  let introspect proxy =
    OBus_method.call m_Introspect proxy ()
end

module Org_freedesktop_DBus_Peer =
struct
  open Org_freedesktop_DBus_Peer


  let ping proxy =
    OBus_method.call m_Ping proxy ()

  let get_machine_id proxy =
    OBus_method.call m_GetMachineId proxy ()
end

module Org_freedesktop_DBus_Properties =
struct
  open Org_freedesktop_DBus_Properties


  let get proxy ~interface_name ~property_name =
    OBus_method.call m_Get proxy (interface_name, property_name)

  let get_all proxy ~interface_name =
    OBus_method.call m_GetAll proxy interface_name

  let set proxy ~interface_name ~property_name ~value =
    OBus_method.call m_Set proxy (interface_name, property_name, value)

  let properties_changed proxy =
    OBus_signal.make s_PropertiesChanged proxy
end

module Org_mpris_MediaPlayer2 =
struct
  open Org_mpris_MediaPlayer2


  let raise proxy =
    OBus_method.call m_Raise proxy ()

  let quit proxy =
    OBus_method.call m_Quit proxy ()

  let can_quit proxy =
    OBus_property.make p_CanQuit proxy

  let can_raise proxy =
    OBus_property.make p_CanRaise proxy

  let has_track_list proxy =
    OBus_property.make p_HasTrackList proxy

  let identity proxy =
    OBus_property.make p_Identity proxy

  let desktop_entry proxy =
    OBus_property.make p_DesktopEntry proxy

  let supported_uri_schemes proxy =
    OBus_property.make p_SupportedUriSchemes proxy

  let supported_mime_types proxy =
    OBus_property.make p_SupportedMimeTypes proxy
end

module Org_mpris_MediaPlayer2_Player =
struct
  open Org_mpris_MediaPlayer2_Player


  let next proxy =
    OBus_method.call m_Next proxy ()

  let previous proxy =
    OBus_method.call m_Previous proxy ()

  let pause proxy =
    OBus_method.call m_Pause proxy ()

  let play_pause proxy =
    OBus_method.call m_PlayPause proxy ()

  let stop proxy =
    OBus_method.call m_Stop proxy ()

  let play proxy =
    OBus_method.call m_Play proxy ()

  let seek proxy ~offset =
    OBus_method.call m_Seek proxy offset

  let set_position proxy ~trackId ~position =
    let trackId = OBus_proxy.path trackId in
    OBus_method.call m_SetPosition proxy (trackId, position)

  let open_uri proxy ~uri =
    OBus_method.call m_OpenUri proxy uri

  let seeked proxy =
    OBus_signal.make s_Seeked proxy

  let playback_status proxy =
    OBus_property.make p_PlaybackStatus proxy

  let loop_status proxy =
    OBus_property.make p_LoopStatus proxy

  let rate proxy =
    OBus_property.make p_Rate proxy

  let shuffle proxy =
    OBus_property.make p_Shuffle proxy

  let metadata proxy =
    OBus_property.make p_Metadata proxy

  let volume proxy =
    OBus_property.make p_Volume proxy

  let position proxy =
    OBus_property.make p_Position proxy

  let minimum_rate proxy =
    OBus_property.make p_MinimumRate proxy

  let maximum_rate proxy =
    OBus_property.make p_MaximumRate proxy

  let can_go_next proxy =
    OBus_property.make p_CanGoNext proxy

  let can_go_previous proxy =
    OBus_property.make p_CanGoPrevious proxy

  let can_play proxy =
    OBus_property.make p_CanPlay proxy

  let can_pause proxy =
    OBus_property.make p_CanPause proxy

  let can_seek proxy =
    OBus_property.make p_CanSeek proxy

  let can_control proxy =
    OBus_property.make p_CanControl proxy
end
