open Ctypes
open Foreign


module Pa = struct
  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#aff56e9b3dd442a88227da084bb5c380a
   *)
  type context
  let context : context structure typ = structure "pa_context"

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/operation_8h.html#a5614a07f2e7a129e4cb16596ed452a0c
   *)
  type operation
  let operation : operation structure typ = structure "pa_operation"

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/sample_8h.html#a41051ceaa5cfbe60c9b176deb7bfed0e
   *)
  type sample_format_t = [
    | `PA_SAMPLE_U8
    | `PA_SAMPLE_ALAW
    | `PA_SAMPLE_ULAW
    | `PA_SAMPLE_S16LE
    | `PA_SAMPLE_S16BE
    | `PA_SAMPLE_FLOAT32LE
    | `PA_SAMPLE_FLOAT32BE
    | `PA_SAMPLE_S32LE
    | `PA_SAMPLE_S32BE
    | `PA_SAMPLE_S24LE
    | `PA_SAMPLE_S24BE
    | `PA_SAMPLE_S24_32LE
    | `PA_SAMPLE_S24_32BE
    | `PA_SAMPLE_MAX
    | `PA_SAMPLE_INVALID
  ]
  let of_int = function
    | 0 -> `PA_SAMPLE_U8
    | 1 -> `PA_SAMPLE_ALAW
    | 2 -> `PA_SAMPLE_ULAW
    | 3 -> `PA_SAMPLE_S16LE
    | 4 -> `PA_SAMPLE_S16BE
    | 5 -> `PA_SAMPLE_FLOAT32LE
    | 6 -> `PA_SAMPLE_FLOAT32BE
    | 7 -> `PA_SAMPLE_S32LE
    | 8 -> `PA_SAMPLE_S32BE
    | 9 -> `PA_SAMPLE_S24LE
    | 10 -> `PA_SAMPLE_S24BE
    | 11 -> `PA_SAMPLE_S24_32LE
    | 12 -> `PA_SAMPLE_S24_32BE
    | 13 -> `PA_SAMPLE_MAX
    | -1 -> `PA_SAMPLE_INVALID
    | _ -> raise (Invalid_argument "Unexpected pa_sample_format_t C enum value")
  let to_int = function
    | `PA_SAMPLE_U8 -> 0
    | `PA_SAMPLE_ALAW -> 1
    | `PA_SAMPLE_ULAW -> 2
    | `PA_SAMPLE_S16LE -> 3
    | `PA_SAMPLE_S16BE -> 4
    | `PA_SAMPLE_FLOAT32LE -> 5
    | `PA_SAMPLE_FLOAT32BE -> 6
    | `PA_SAMPLE_S32LE -> 7
    | `PA_SAMPLE_S32BE -> 8
    | `PA_SAMPLE_S24LE -> 9
    | `PA_SAMPLE_S24BE -> 10
    | `PA_SAMPLE_S24_32LE -> 11
    | `PA_SAMPLE_S24_32BE -> 12
    | `PA_SAMPLE_MAX -> 13
    | `PA_SAMPLE_INVALID -> -1
  let sample_format_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/channelmap_8h.html#af1cbe2738487c74f99e613779bd34bf2
   *)
  type channel_position_t = [
    | `PA_CHANNEL_POSITION_INVALID
    | `PA_CHANNEL_POSITION_MONO
    | `PA_CHANNEL_POSITION_FRONT_LEFT
    | `PA_CHANNEL_POSITION_FRONT_RIGHT
    | `PA_CHANNEL_POSITION_FRONT_CENTER
    | `PA_CHANNEL_POSITION_REAR_CENTER
    | `PA_CHANNEL_POSITION_REAR_LEFT
    | `PA_CHANNEL_POSITION_REAR_RIGHT
    | `PA_CHANNEL_POSITION_LFE
    | `PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
    | `PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
    | `PA_CHANNEL_POSITION_SIDE_LEFT
    | `PA_CHANNEL_POSITION_SIDE_RIGHT
    | `PA_CHANNEL_POSITION_AUX0
    | `PA_CHANNEL_POSITION_AUX1
    | `PA_CHANNEL_POSITION_AUX2
    | `PA_CHANNEL_POSITION_AUX3
    | `PA_CHANNEL_POSITION_AUX4
    | `PA_CHANNEL_POSITION_AUX5
    | `PA_CHANNEL_POSITION_AUX6
    | `PA_CHANNEL_POSITION_AUX7
    | `PA_CHANNEL_POSITION_AUX8
    | `PA_CHANNEL_POSITION_AUX9
    | `PA_CHANNEL_POSITION_AUX10
    | `PA_CHANNEL_POSITION_AUX11
    | `PA_CHANNEL_POSITION_AUX12
    | `PA_CHANNEL_POSITION_AUX13
    | `PA_CHANNEL_POSITION_AUX14
    | `PA_CHANNEL_POSITION_AUX15
    | `PA_CHANNEL_POSITION_AUX16
    | `PA_CHANNEL_POSITION_AUX17
    | `PA_CHANNEL_POSITION_AUX18
    | `PA_CHANNEL_POSITION_AUX19
    | `PA_CHANNEL_POSITION_AUX20
    | `PA_CHANNEL_POSITION_AUX21
    | `PA_CHANNEL_POSITION_AUX22
    | `PA_CHANNEL_POSITION_AUX23
    | `PA_CHANNEL_POSITION_AUX24
    | `PA_CHANNEL_POSITION_AUX25
    | `PA_CHANNEL_POSITION_AUX26
    | `PA_CHANNEL_POSITION_AUX27
    | `PA_CHANNEL_POSITION_AUX28
    | `PA_CHANNEL_POSITION_AUX29
    | `PA_CHANNEL_POSITION_AUX30
    | `PA_CHANNEL_POSITION_AUX31
    | `PA_CHANNEL_POSITION_TOP_CENTER
    | `PA_CHANNEL_POSITION_TOP_FRONT_LEFT
    | `PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
    | `PA_CHANNEL_POSITION_TOP_FRONT_CENTER
    | `PA_CHANNEL_POSITION_TOP_REAR_LEFT
    | `PA_CHANNEL_POSITION_TOP_REAR_RIGHT
    | `PA_CHANNEL_POSITION_TOP_REAR_CENTER
    | `PA_CHANNEL_POSITION_MAX
  ]
  let of_int = function
    | -1 -> `PA_CHANNEL_POSITION_INVALID
    | 0 -> `PA_CHANNEL_POSITION_MONO
    | 1 -> `PA_CHANNEL_POSITION_FRONT_LEFT
    | 2 -> `PA_CHANNEL_POSITION_FRONT_RIGHT
    | 3 -> `PA_CHANNEL_POSITION_FRONT_CENTER
    | 4 -> `PA_CHANNEL_POSITION_REAR_CENTER
    | 5 -> `PA_CHANNEL_POSITION_REAR_LEFT
    | 6 -> `PA_CHANNEL_POSITION_REAR_RIGHT
    | 7 -> `PA_CHANNEL_POSITION_LFE
    | 8 -> `PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
    | 9 -> `PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
    | 10 -> `PA_CHANNEL_POSITION_SIDE_LEFT
    | 11 -> `PA_CHANNEL_POSITION_SIDE_RIGHT
    | 12 -> `PA_CHANNEL_POSITION_AUX0
    | 13 -> `PA_CHANNEL_POSITION_AUX1
    | 14 -> `PA_CHANNEL_POSITION_AUX2
    | 15 -> `PA_CHANNEL_POSITION_AUX3
    | 16 -> `PA_CHANNEL_POSITION_AUX4
    | 17 -> `PA_CHANNEL_POSITION_AUX5
    | 18 -> `PA_CHANNEL_POSITION_AUX6
    | 19 -> `PA_CHANNEL_POSITION_AUX7
    | 20 -> `PA_CHANNEL_POSITION_AUX8
    | 21 -> `PA_CHANNEL_POSITION_AUX9
    | 22 -> `PA_CHANNEL_POSITION_AUX10
    | 23 -> `PA_CHANNEL_POSITION_AUX11
    | 24 -> `PA_CHANNEL_POSITION_AUX12
    | 25 -> `PA_CHANNEL_POSITION_AUX13
    | 26 -> `PA_CHANNEL_POSITION_AUX14
    | 27 -> `PA_CHANNEL_POSITION_AUX15
    | 28 -> `PA_CHANNEL_POSITION_AUX16
    | 29 -> `PA_CHANNEL_POSITION_AUX17
    | 30 -> `PA_CHANNEL_POSITION_AUX18
    | 31 -> `PA_CHANNEL_POSITION_AUX19
    | 32 -> `PA_CHANNEL_POSITION_AUX20
    | 33 -> `PA_CHANNEL_POSITION_AUX21
    | 34 -> `PA_CHANNEL_POSITION_AUX22
    | 35 -> `PA_CHANNEL_POSITION_AUX23
    | 36 -> `PA_CHANNEL_POSITION_AUX24
    | 37 -> `PA_CHANNEL_POSITION_AUX25
    | 38 -> `PA_CHANNEL_POSITION_AUX26
    | 39 -> `PA_CHANNEL_POSITION_AUX27
    | 40 -> `PA_CHANNEL_POSITION_AUX28
    | 41 -> `PA_CHANNEL_POSITION_AUX29
    | 42 -> `PA_CHANNEL_POSITION_AUX30
    | 43 -> `PA_CHANNEL_POSITION_AUX31
    | 44 -> `PA_CHANNEL_POSITION_TOP_CENTER
    | 45 -> `PA_CHANNEL_POSITION_TOP_FRONT_LEFT
    | 46 -> `PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
    | 47 -> `PA_CHANNEL_POSITION_TOP_FRONT_CENTER
    | 48 -> `PA_CHANNEL_POSITION_TOP_REAR_LEFT
    | 49 -> `PA_CHANNEL_POSITION_TOP_REAR_RIGHT
    | 50 -> `PA_CHANNEL_POSITION_TOP_REAR_CENTER
    | 51 -> `PA_CHANNEL_POSITION_MAX
    | _ -> raise (Invalid_argument "Unexpected pa_channel_position_t C enum value")
  let to_int = function
    | `PA_CHANNEL_POSITION_INVALID -> -1
    | `PA_CHANNEL_POSITION_MONO -> 0
    | `PA_CHANNEL_POSITION_FRONT_LEFT -> 1
    | `PA_CHANNEL_POSITION_FRONT_RIGHT -> 2
    | `PA_CHANNEL_POSITION_FRONT_CENTER -> 3
    | `PA_CHANNEL_POSITION_REAR_CENTER -> 4
    | `PA_CHANNEL_POSITION_REAR_LEFT -> 5
    | `PA_CHANNEL_POSITION_REAR_RIGHT -> 6
    | `PA_CHANNEL_POSITION_LFE -> 7
    | `PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER -> 8
    | `PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER -> 9
    | `PA_CHANNEL_POSITION_SIDE_LEFT -> 10
    | `PA_CHANNEL_POSITION_SIDE_RIGHT -> 11
    | `PA_CHANNEL_POSITION_AUX0 -> 12
    | `PA_CHANNEL_POSITION_AUX1 -> 13
    | `PA_CHANNEL_POSITION_AUX2 -> 14
    | `PA_CHANNEL_POSITION_AUX3 -> 15
    | `PA_CHANNEL_POSITION_AUX4 -> 16
    | `PA_CHANNEL_POSITION_AUX5 -> 17
    | `PA_CHANNEL_POSITION_AUX6 -> 18
    | `PA_CHANNEL_POSITION_AUX7 -> 19
    | `PA_CHANNEL_POSITION_AUX8 -> 20
    | `PA_CHANNEL_POSITION_AUX9 -> 21
    | `PA_CHANNEL_POSITION_AUX10 -> 22
    | `PA_CHANNEL_POSITION_AUX11 -> 23
    | `PA_CHANNEL_POSITION_AUX12 -> 24
    | `PA_CHANNEL_POSITION_AUX13 -> 25
    | `PA_CHANNEL_POSITION_AUX14 -> 26
    | `PA_CHANNEL_POSITION_AUX15 -> 27
    | `PA_CHANNEL_POSITION_AUX16 -> 28
    | `PA_CHANNEL_POSITION_AUX17 -> 29
    | `PA_CHANNEL_POSITION_AUX18 -> 30
    | `PA_CHANNEL_POSITION_AUX19 -> 31
    | `PA_CHANNEL_POSITION_AUX20 -> 32
    | `PA_CHANNEL_POSITION_AUX21 -> 33
    | `PA_CHANNEL_POSITION_AUX22 -> 34
    | `PA_CHANNEL_POSITION_AUX23 -> 35
    | `PA_CHANNEL_POSITION_AUX24 -> 36
    | `PA_CHANNEL_POSITION_AUX25 -> 37
    | `PA_CHANNEL_POSITION_AUX26 -> 38
    | `PA_CHANNEL_POSITION_AUX27 -> 39
    | `PA_CHANNEL_POSITION_AUX28 -> 40
    | `PA_CHANNEL_POSITION_AUX29 -> 41
    | `PA_CHANNEL_POSITION_AUX30 -> 42
    | `PA_CHANNEL_POSITION_AUX31 -> 43
    | `PA_CHANNEL_POSITION_TOP_CENTER -> 44
    | `PA_CHANNEL_POSITION_TOP_FRONT_LEFT -> 45
    | `PA_CHANNEL_POSITION_TOP_FRONT_RIGHT -> 46
    | `PA_CHANNEL_POSITION_TOP_FRONT_CENTER -> 47
    | `PA_CHANNEL_POSITION_TOP_REAR_LEFT -> 48
    | `PA_CHANNEL_POSITION_TOP_REAR_RIGHT -> 49
    | `PA_CHANNEL_POSITION_TOP_REAR_CENTER -> 50
    | `PA_CHANNEL_POSITION_MAX -> 51
  let channel_position_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  let channels_max = 32

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__channel__map.html
   *)
  module Channel_map = struct
    type t
    let channel_map : t structure typ = structure "pa_channel_map"
    let map_channels = field channel_map "channels" uint8_t
    let map = field channel_map "map" (array channels_max channel_position_t)
    let () = seal channel_map
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__sample__spec.html
   *)
  module Sample_spec = struct
    type t
    let sample_spec : t structure typ = structure "pa_sample_spec"
    let format = field sample_spec "format" sample_format_t
    let rate = field sample_spec "rate" uint32_t
    let channels = field sample_spec "channels" uint8_t
    let () = seal sample_spec
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__server__info.html
   *)
  module Server_info = struct
    type t
    let server_info : t structure typ = structure "pa_server_info"
    let user_name = field server_info "user_name" string
    let host_name = field server_info "host_name" string
    let server_version = field server_info "server_version" string
    let server_name = field server_info "server_name" string
    let sample_spec = field server_info "sample_spec" Sample_spec.sample_spec
    let default_sink_name = field server_info "default_sink_name" string
    let default_source_name = field server_info "default_source_name" string
    let cookie = field server_info "cookie" uint32_t
    let channel_map = field server_info "channel_map" Channel_map.channel_map
    let () = seal server_info
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/introspect_8h.html#a7ed151f598cdbdd52d2841b313984690
   * typedef void( *pa_server_info_cb_t) (pa_context* c, const pa_server_info* i, void* userdata)
   *)
  let server_info_cb_t = ptr context @-> ptr Server_info.server_info @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/glib-mainloop_8h.html#a8bc9192e5e69df72dd3a6a8fdaaca92f
   *)
  type glib_mainloop
  let glib_mainloop : glib_mainloop structure typ = structure "pa_glib_mainloop"

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__mainloop__api.html
   *)
  type mainloop_api
  let mainloop_api : mainloop_api structure typ = structure "pa_mainloop_api"

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/mainloop-signal_8h.html#a8346c68814daec286cef332fa9df302c
   *)
  type signal_event
  let signal_event : signal_event structure typ = structure "pa_signal_event"

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/mainloop-signal_8h.html#a7d658f2cba34a9fa27d7270269b5d88e
   * typedef void( *pa_signal_cb_t) (pa_mainloop_api *api, pa_signal_event *e, int sig, void *userdata)
   *)
  let signal_cb_t = ptr mainloop_api @-> ptr signal_event @-> int @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a19074e289d91ccae8dd58d15e912fc13
   * typedef void( *pa_context_notify_cb_t) (pa_context* c, void* userdata)
   *)
  let context_notify_cb_t = ptr context @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#aa330581bb9b282e6f6a28a3a2b4b634a
   * typedef enum pa_context_flags pa_context_flags_t
   *)
  type context_flags_t = [
    | `PA_CONTEXT_NOFLAGS
    | `PA_CONTEXT_NOAUTOSPAWN
    | `PA_CONTEXT_NOFAIL
  ]
  let of_int = function
    | 0 -> `PA_CONTEXT_NOFLAGS
    | 1 -> `PA_CONTEXT_NOAUTOSPAWN
    | 2 -> `PA_CONTEXT_NOFAIL
    | _ -> raise (Invalid_argument "Unexpected pa_context_flags C enum value")
  let to_int = function
    | `PA_CONTEXT_NOFLAGS -> 0
    | `PA_CONTEXT_NOAUTOSPAWN -> 1
    | `PA_CONTEXT_NOFAIL -> 2
  let context_flags_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#a73eb0e01743b361a0b21e767655f23b2
   * pa_error_code_t
   *)
  type error_code_t = [
    | `PA_OK
    | `PA_ERR_ACCESS
    | `PA_ERR_COMMAND
    | `PA_ERR_INVALID
    | `PA_ERR_EXIST
    | `PA_ERR_NOENTITY
    | `PA_ERR_CONNECTIONREFUSED
    | `PA_ERR_PROTOCOL
    | `PA_ERR_TIMEOUT
    | `PA_ERR_AUTHKEY
    | `PA_ERR_INTERNAL
    | `PA_ERR_CONNECTIONTERMINATED
    | `PA_ERR_KILLED
    | `PA_ERR_INVALIDSERVER
    | `PA_ERR_MODINITFAILED
    | `PA_ERR_BADSTATE
    | `PA_ERR_NODATA
    | `PA_ERR_VERSION
    | `PA_ERR_TOOLARGE
    | `PA_ERR_NOTSUPPORTED
    | `PA_ERR_UNKNOWN
    | `PA_ERR_NOEXTENSION
    | `PA_ERR_OBSOLETE
    | `PA_ERR_NOTIMPLEMENTED
    | `PA_ERR_FORKED
    | `PA_ERR_IO
    | `PA_ERR_BUSY
    | `PA_ERR_MAX
  ]
  let of_int = function
    | 0 -> `PA_OK
    | 1 -> `PA_ERR_ACCESS
    | 2 -> `PA_ERR_COMMAND
    | 3 -> `PA_ERR_INVALID
    | 4 -> `PA_ERR_EXIST
    | 5 -> `PA_ERR_NOENTITY
    | 6 -> `PA_ERR_CONNECTIONREFUSED
    | 7 -> `PA_ERR_PROTOCOL
    | 8 -> `PA_ERR_TIMEOUT
    | 9 -> `PA_ERR_AUTHKEY
    | 10 -> `PA_ERR_INTERNAL
    | 11 -> `PA_ERR_CONNECTIONTERMINATED
    | 12 -> `PA_ERR_KILLED
    | 13 -> `PA_ERR_INVALIDSERVER
    | 14 -> `PA_ERR_MODINITFAILED
    | 15 -> `PA_ERR_BADSTATE
    | 16 -> `PA_ERR_NODATA
    | 17 -> `PA_ERR_VERSION
    | 18 -> `PA_ERR_TOOLARGE
    | 19 -> `PA_ERR_NOTSUPPORTED
    | 20 -> `PA_ERR_UNKNOWN
    | 21 -> `PA_ERR_NOEXTENSION
    | 22 -> `PA_ERR_OBSOLETE
    | 23 -> `PA_ERR_NOTIMPLEMENTED
    | 24 -> `PA_ERR_FORKED
    | 25 -> `PA_ERR_IO
    | 26 -> `PA_ERR_BUSY
    | 27 -> `PA_ERR_MAX
    | _ -> raise (Invalid_argument "Unexpected pa_error_code_t C enum value")
  let to_int = function
    | `PA_OK -> 0
    | `PA_ERR_ACCESS -> 1
    | `PA_ERR_COMMAND -> 2
    | `PA_ERR_INVALID -> 3
    | `PA_ERR_EXIST -> 4
    | `PA_ERR_NOENTITY -> 5
    | `PA_ERR_CONNECTIONREFUSED -> 6
    | `PA_ERR_PROTOCOL -> 7
    | `PA_ERR_TIMEOUT -> 8
    | `PA_ERR_AUTHKEY -> 9
    | `PA_ERR_INTERNAL -> 10
    | `PA_ERR_CONNECTIONTERMINATED -> 11
    | `PA_ERR_KILLED -> 12
    | `PA_ERR_INVALIDSERVER -> 13
    | `PA_ERR_MODINITFAILED -> 14
    | `PA_ERR_BADSTATE -> 15
    | `PA_ERR_NODATA -> 16
    | `PA_ERR_VERSION -> 17
    | `PA_ERR_TOOLARGE -> 18
    | `PA_ERR_NOTSUPPORTED -> 19
    | `PA_ERR_UNKNOWN -> 20
    | `PA_ERR_NOEXTENSION -> 21
    | `PA_ERR_OBSOLETE -> 22
    | `PA_ERR_NOTIMPLEMENTED -> 23
    | `PA_ERR_FORKED -> 24
    | `PA_ERR_IO -> 25
    | `PA_ERR_BUSY -> 26
    | `PA_ERR_MAX -> 27
  let error_code_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#a892684c03cf9edaed1a95e609ec7573c
   *)
  type context_state_t = [
    | `PA_CONTEXT_UNCONNECTED
    | `PA_CONTEXT_CONNECTING
    | `PA_CONTEXT_AUTHORIZING
    | `PA_CONTEXT_SETTING_NAME
    | `PA_CONTEXT_READY
    | `PA_CONTEXT_FAILED
    | `PA_CONTEXT_TERMINATED
  ]
  let of_int = function
    | 0 -> `PA_CONTEXT_UNCONNECTED
    | 1 -> `PA_CONTEXT_CONNECTING
    | 2 -> `PA_CONTEXT_AUTHORIZING
    | 3 -> `PA_CONTEXT_SETTING_NAME
    | 4 -> `PA_CONTEXT_READY
    | 5 -> `PA_CONTEXT_FAILED
    | 6 -> `PA_CONTEXT_TERMINATED
    | _ -> raise (Invalid_argument "Unexpected pa_context_state_t C enum value")
  let to_int = function
    | `PA_CONTEXT_UNCONNECTED -> 0
    | `PA_CONTEXT_CONNECTING -> 1
    | `PA_CONTEXT_AUTHORIZING -> 2
    | `PA_CONTEXT_SETTING_NAME -> 3
    | `PA_CONTEXT_READY -> 4
    | `PA_CONTEXT_FAILED -> 5
    | `PA_CONTEXT_TERMINATED -> 6
  let context_state_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/glib-mainloop_8h.html#a52cb5213e3fd22e4e387b69939fee867
   * pa_glib_mainloop* pa_glib_mainloop_new(GMainContext* c)
   *)
  let glib_mainloop_new = foreign "pa_glib_mainloop_new" ((ptr Glib.g_main_context) @-> returning (ptr glib_mainloop))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/glib-mainloop_8h.html#a44580affe016663d24f0842948b24e3f
   * void pa_glib_mainloop_free(pa_glib_mainloop* g)
   *)
  let glib_mainloop_free = foreign "pa_glib_mainloop_free" (ptr glib_mainloop @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/glib-mainloop_8h.html#afd4ea5a35ba9b05032ae9cbd6e55b8b4
   * pa_mainloop_api* pa_glib_mainloop_get_api(pa_glib_mainloop* g)
   *)
  let glib_mainloop_get_api = foreign "pa_glib_mainloop_get_api" ((ptr glib_mainloop) @-> returning (ptr mainloop_api))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/mainloop-signal_8h.html#a08bd75b482aabc45114dbcb53f004fe6
   * int pa_signal_init(pa_mainloop_api* api)
   *)
  let signal_init = foreign "pa_signal_init" ((ptr mainloop_api) @-> returning int)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/mainloop-signal_8h.html#ac1648b4c7046eea5809e21838b604d12
   * pa_signal_event* pa_signal_new(int sig, pa_signal_cb_t callback, void* userdata)
   *)
  let signal_new = foreign "pa_signal_new" (int @-> funptr signal_cb_t @-> ptr void @-> returning (ptr signal_event))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a2784c754947a97f02c78b73d7b1c2d5f
   * pa_context* pa_context_new(pa_mainloop_api* mainloop, const char* name)
   *)
  let context_new = foreign "pa_context_new" (ptr mainloop_api @-> string @-> returning (ptr context))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#ad6017a90c6669856a0196ca96b0f2d35
   * pa_context_state_t pa_context_get_state(const pa_context* c)
   *)
  let context_get_state = foreign "pa_context_get_state" (ptr context @-> returning context_state_t)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a154b9d8057adfbb2cecfbd9406a27660
   * void pa_context_disconnect(pa_context* c)
   *)
  let context_disconnect = foreign "pa_context_disconnect" (ptr context @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#aeb7b395fd3a345fc181d6bfcdbe5f3d8
   * void pa_context_set_state_callback(pa_context* c, pa_context_notify_cb_t cb, void* userdata)
   *)
  let context_set_state_callback = foreign "pa_context_set_state_callback"
    (ptr context @-> funptr context_notify_cb_t @-> ptr void @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a983ce13d45c5f4b0db8e1a34e21f9fce
   * int pa_context_connect (pa_context* c, const char* server, pa_context_flags_t flags, const pa_spawn_api* api)
   *)
  let context_connect = foreign "pa_context_connect"
    (ptr context @-> ptr void @-> context_flags_t @-> ptr void @-> returning int)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a344c4ccf14d6a8842e83154a0aa99311
   * void pa_context_unref(pa_context* c)
   *)
  let context_unref = foreign "pa_context_unref" (ptr context @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/operation_8h.html#a8d2ef9bb2ff961ee31675882bf125227
   * void pa_operation_unref(pa_operation* o)
   *)
  let operation_unref = foreign "pa_operation_unref" (ptr operation @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a663ad2acbf708102dc2deee2801f2441
   * int pa_context_errno (const pa_context* c)
   *)
  let context_errno = foreign "pa_context_errno" (ptr context @-> returning error_code_t)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/introspect_8h.html#a6f3bdd9982d9dec70ca5acf6ada3a9a2
   * pa_operation* pa_context_get_server_info(pa_context* c, pa_server_info_cb_t cb, void* userdata)
   *)
  let context_get_server_info = foreign "pa_context_get_server_info"
    (ptr context @-> funptr server_info_cb_t @-> ptr void @-> returning (ptr operation))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__cvolume.html
   *)
  module Cvolume = struct
    type t
    let cvolume : t structure typ = structure "pa_cvolume"
    let channels = field cvolume "channels" uint8_t
    let values = field cvolume "values" (array channels_max uint32_t)
    let () = seal cvolume
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#a20e0a15bebf78a29893f3b73ef7dadc0
   *)
  type sink_flags_t = [
    | `PA_SINK_NOFLAGS
    | `PA_SINK_HW_VOLUME_CTRL
    | `PA_SINK_LATENCY
    | `PA_SINK_HARDWARE
    | `PA_SINK_NETWORK
    | `PA_SINK_HW_MUTE_CTRL
    | `PA_SINK_DECIBEL_VOLUME
    | `PA_SINK_FLAT_VOLUME
    | `PA_SINK_DYNAMIC_LATENCY
    | `PA_SINK_SET_FORMATS
  ]
  let of_int = function
    | 0x0000 -> `PA_SINK_NOFLAGS
    | 0x0001 -> `PA_SINK_HW_VOLUME_CTRL
    | 0x0002 -> `PA_SINK_LATENCY
    | 0x0004 -> `PA_SINK_HARDWARE
    | 0x0008 -> `PA_SINK_NETWORK
    | 0x0010 -> `PA_SINK_HW_MUTE_CTRL
    | 0x0020 -> `PA_SINK_DECIBEL_VOLUME
    | 0x0040 -> `PA_SINK_FLAT_VOLUME
    | 0x0080 -> `PA_SINK_DYNAMIC_LATENCY
    | 0x0100 -> `PA_SINK_SET_FORMATS
    | _ -> raise (Invalid_argument "Unexpected pa_sink_flags_t C enum value")
  let to_int = function
    | `PA_SINK_NOFLAGS -> 0x0000
    | `PA_SINK_HW_VOLUME_CTRL -> 0x0001
    | `PA_SINK_LATENCY -> 0x0002
    | `PA_SINK_HARDWARE -> 0x0004
    | `PA_SINK_NETWORK -> 0x0008
    | `PA_SINK_HW_MUTE_CTRL -> 0x0010
    | `PA_SINK_DECIBEL_VOLUME -> 0x0020
    | `PA_SINK_FLAT_VOLUME -> 0x0040
    | `PA_SINK_DYNAMIC_LATENCY -> 0x0080
    | `PA_SINK_SET_FORMATS -> 0x0100
  let sink_flags_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#ae4618f4ca1ed5b4044390f6421ac233e
   *)
  type sink_state_t = [
    | `PA_SINK_INVALID_STATE
    | `PA_SINK_RUNNING
    | `PA_SINK_IDLE
    | `PA_SINK_SUSPENDED
  ]
  let of_int = function
    | -1 -> `PA_SINK_INVALID_STATE
    |  0 -> `PA_SINK_RUNNING
    |  1 -> `PA_SINK_IDLE
    |  2 -> `PA_SINK_SUSPENDED
    | _ -> raise (Invalid_argument "Unexpected pa_sink_state_t C enum value")
  let to_int = function
    | `PA_SINK_INVALID_STATE -> -1
    | `PA_SINK_RUNNING -> 0
    | `PA_SINK_IDLE -> 1
    | `PA_SINK_SUSPENDED -> 2
  let sink_state_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/structpa__sink__info.html
   *)
  module Sink_info = struct
    type t
    let sink_info : t structure typ = structure "pa_sink_info"
    let name = field sink_info "name" string
    let index = field sink_info "index" uint8_t
    let description = field sink_info "description" string
    let sample_spec = field sink_info "sample_spec" Sample_spec.sample_spec
    let channel_map = field sink_info "channel_map" Channel_map.channel_map
    let owner_module = field sink_info "owner_module" uint32_t
    let volume = field sink_info "volume" Cvolume.cvolume
    let mute = field sink_info "mute" int
    let monitor_source = field sink_info "monitor_source" uint32_t
    let monitor_source_name = field sink_info "monitor_source_name" string
    let latency = field sink_info "latency" uint64_t
    let driver = field sink_info "driver" string
    let flags = field sink_info "flags" sink_flags_t
    let proplist = field sink_info "proplist" (ptr void) (* C type = "pa_proplist*" but unused in this binding *)
    let configured_latency = field sink_info "configured_latency" uint64_t
    let base_volume = field sink_info "base_volume" uint32_t
    let state = field sink_info "state" sink_state_t
    let n_volume_steps = field sink_info "n_volume_steps" uint32_t
    let card = field sink_info "card" uint32_t
    let n_ports = field sink_info "n_ports" uint32_t
    let ports = field sink_info "ports" (ptr void) (* C type = "pa_sink_port_info**" but unused in this binding *)
    let active_port = field sink_info "active_port" (ptr void) (* C type = "pa_sink_port_info*" but unused in this binding *)
    let n_formats = field sink_info "n_formats" uint8_t
    let formats = field sink_info "formats" (ptr void) (* C type = "pa_format_info**" but unused in this binding *)
    let () = seal sink_info
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/introspect_8h.html#acf7af674e3cee1ec7f817190a2d2702d
   * typedef void( *pa_sink_info_cb_t) (pa_context *c, const pa_sink_info *i, int eol, void *userdata)
   *)
  let sink_info_cb_t = ptr context @-> ptr Sink_info.sink_info @-> int @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/introspect_8h.html#a446ffde2b8adea89940adcba40be319c
   * pa_operation* pa_context_get_sink_info_by_name(pa_context* c, const char* name, pa_sink_info_cb_t cb, void* userdata)
   *)
  let context_get_sink_info_by_name = foreign "pa_context_get_sink_info_by_name"
    (ptr context @-> string @-> funptr sink_info_cb_t @-> ptr void @-> returning (ptr operation))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/introspect_8h.html#ae886cd4bdc06fa98a61d15f818c33c9f
   * pa_operation* pa_context_get_sink_info_by_index(pa_context* c, uint32_t idx, pa_sink_info_cb_t cb, void* userdata)
   *)
  let context_get_sink_info_by_index = foreign "pa_context_get_sink_info_by_index"
    (ptr context @-> uint32_t @-> funptr sink_info_cb_t @-> ptr void @-> returning (ptr operation))

  let volume_norm = 0x10000

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/volume_8h.html#a0f34f2c6d1b4738bf7f11ff06775dc1f
   * pa_volume_t pa_cvolume_avg(const pa_cvolume* a)
   *)
  let cvolume_avg = foreign "pa_cvolume_avg" ((ptr Cvolume.cvolume) @-> returning uint32_t)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#acbc7a15d7a9fe0722b02e2d739200035
   *)
  module Subscription_event_type_t = struct
    type t = [
      | `PA_SUBSCRIPTION_EVENT_SINK
      | `PA_SUBSCRIPTION_EVENT_SOURCE
      | `PA_SUBSCRIPTION_EVENT_SINK_INPUT
      | `PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT
      | `PA_SUBSCRIPTION_EVENT_MODULE
      | `PA_SUBSCRIPTION_EVENT_CLIENT
      | `PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE
      | `PA_SUBSCRIPTION_EVENT_SERVER
      | `PA_SUBSCRIPTION_EVENT_CARD
      | `PA_SUBSCRIPTION_EVENT_FACILITY_MASK
      | `PA_SUBSCRIPTION_EVENT_NEW
      | `PA_SUBSCRIPTION_EVENT_CHANGE
      | `PA_SUBSCRIPTION_EVENT_REMOVE
      | `PA_SUBSCRIPTION_EVENT_TYPE_MASK
    ]
    let of_int : int -> t = function
      | 0x0000 -> `PA_SUBSCRIPTION_EVENT_SINK
      | 0x0001 -> `PA_SUBSCRIPTION_EVENT_SOURCE
      | 0x0002 -> `PA_SUBSCRIPTION_EVENT_SINK_INPUT
      | 0x0003 -> `PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT
      | 0x0004 -> `PA_SUBSCRIPTION_EVENT_MODULE
      | 0x0005 -> `PA_SUBSCRIPTION_EVENT_CLIENT
      | 0x0006 -> `PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE
      | 0x0007 -> `PA_SUBSCRIPTION_EVENT_SERVER
      | 0x0009 -> `PA_SUBSCRIPTION_EVENT_CARD
      | 0x000F -> `PA_SUBSCRIPTION_EVENT_FACILITY_MASK
      (* | 0x0000 -> `PA_SUBSCRIPTION_EVENT_NEW  duplicated in PulseAudio source code!
       * https://github.com/pulseaudio/pulseaudio/blob/4e3a080d7699732be9c522be9a96d851f97fbf11/src/pulse/def.h#L609
       *)
      | 0x0010 -> `PA_SUBSCRIPTION_EVENT_CHANGE
      | 0x0020 -> `PA_SUBSCRIPTION_EVENT_REMOVE
      | 0x0030 -> `PA_SUBSCRIPTION_EVENT_TYPE_MASK
      | _ -> raise (Invalid_argument "Unexpected pa_subscription_event_type_t C enum value")
    let to_int : t -> int = function
      | `PA_SUBSCRIPTION_EVENT_SINK -> 0x0000
      | `PA_SUBSCRIPTION_EVENT_SOURCE -> 0x0001
      | `PA_SUBSCRIPTION_EVENT_SINK_INPUT -> 0x0002
      | `PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT -> 0x0003
      | `PA_SUBSCRIPTION_EVENT_MODULE -> 0x0004
      | `PA_SUBSCRIPTION_EVENT_CLIENT -> 0x0005
      | `PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE -> 0x0006
      | `PA_SUBSCRIPTION_EVENT_SERVER -> 0x0007
      | `PA_SUBSCRIPTION_EVENT_CARD -> 0x0009
      | `PA_SUBSCRIPTION_EVENT_FACILITY_MASK -> 0x000F
      | `PA_SUBSCRIPTION_EVENT_NEW -> 0x0000
      | `PA_SUBSCRIPTION_EVENT_CHANGE -> 0x0010
      | `PA_SUBSCRIPTION_EVENT_REMOVE -> 0x0020
      | `PA_SUBSCRIPTION_EVENT_TYPE_MASK -> 0x0030
    let subscription_event_type_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int
  end

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/subscribe_8h.html#ad1e70a8a270c955487280620ab0a2f0f
   * typedef void( *pa_context_subscribe_cb_t) (pa_context *c, pa_subscription_event_type_t t, uint32_t idx, void *userdata)
   *)
  let context_subscribe_cb_t = ptr context @-> Subscription_event_type_t.subscription_event_type_t @-> uint32_t @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/subscribe_8h.html#a55281f798863e7b37594d347be7ad98c
   * void pa_context_set_subscribe_callback (pa_context* c, pa_context_subscribe_cb_t cb, void* userdata)
   *)
  let context_set_subscribe_callback = foreign "pa_context_set_subscribe_callback"
    (ptr context @-> funptr context_subscribe_cb_t @-> ptr void @-> returning void)

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/context_8h.html#a086b985c499c50efc628c267e8923fb1
   * typedef void( *pa_context_success_cb_t) (pa_context *c, int success, void *userdata)
   *)
  let context_success_cb_t = ptr context @-> int @-> ptr void @-> returning void

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/def_8h.html#ad4e7f11f879e8c77ae5289145ecf6947
   *)
  type subscription_mask_t = [
    | `PA_SUBSCRIPTION_MASK_NULL
    | `PA_SUBSCRIPTION_MASK_SINK
    | `PA_SUBSCRIPTION_MASK_SOURCE
    | `PA_SUBSCRIPTION_MASK_SINK_INPUT
    | `PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT
    | `PA_SUBSCRIPTION_MASK_MODULE
    | `PA_SUBSCRIPTION_MASK_CLIENT
    | `PA_SUBSCRIPTION_MASK_SAMPLE_CACHE
    | `PA_SUBSCRIPTION_MASK_SERVER
    | `PA_SUBSCRIPTION_MASK_CARD
    | `PA_SUBSCRIPTION_MASK_ALL
  ]
  let of_int = function
    | 0x0000 -> `PA_SUBSCRIPTION_MASK_NULL
    | 0x0001 -> `PA_SUBSCRIPTION_MASK_SINK
    | 0x0002 -> `PA_SUBSCRIPTION_MASK_SOURCE
    | 0x0004 -> `PA_SUBSCRIPTION_MASK_SINK_INPUT
    | 0x0008 -> `PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT
    | 0x0010 -> `PA_SUBSCRIPTION_MASK_MODULE
    | 0x0020 -> `PA_SUBSCRIPTION_MASK_CLIENT
    | 0x0040 -> `PA_SUBSCRIPTION_MASK_SAMPLE_CACHE
    | 0x0080 -> `PA_SUBSCRIPTION_MASK_SERVER
    | 0x0200 -> `PA_SUBSCRIPTION_MASK_CARD
    | 0x02ff -> `PA_SUBSCRIPTION_MASK_ALL
    | _ -> raise (Invalid_argument "Unexpected pa_subscription_mask_t C enum value")
  let to_int = function
    | `PA_SUBSCRIPTION_MASK_NULL -> 0x0000
    | `PA_SUBSCRIPTION_MASK_SINK -> 0x0001
    | `PA_SUBSCRIPTION_MASK_SOURCE -> 0x0002
    | `PA_SUBSCRIPTION_MASK_SINK_INPUT -> 0x0004
    | `PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT -> 0x0008
    | `PA_SUBSCRIPTION_MASK_MODULE -> 0x0010
    | `PA_SUBSCRIPTION_MASK_CLIENT -> 0x0020
    | `PA_SUBSCRIPTION_MASK_SAMPLE_CACHE -> 0x0040
    | `PA_SUBSCRIPTION_MASK_SERVER -> 0x0080
    | `PA_SUBSCRIPTION_MASK_CARD -> 0x0200
    | `PA_SUBSCRIPTION_MASK_ALL -> 0x02ff
  let subscription_mask_t = Ctypes.view ~read:of_int ~write:to_int Ctypes.int

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/subscribe_8h.html#abe684246fd5cb640b0199bcfe7f801b0
   * pa_operation* pa_context_subscribe(pa_context* c, pa_subscription_mask_t m, pa_context_success_cb_t cb, void* userdata)
   *)
  let context_subscribe = foreign "pa_context_subscribe"
    (ptr context @-> subscription_mask_t @-> ptr void @-> ptr void @-> returning (ptr operation))

  (*
   * https://freedesktop.org/software/pulseaudio/doxygen/mainloop-signal_8h.html#ab5a95e3bcd871864274083aea58a60fa
   * void pa_signal_free(pa_signal_event* e)
   *)
  let signal_free = foreign "pa_signal_free" (ptr signal_event @-> returning void)
end
