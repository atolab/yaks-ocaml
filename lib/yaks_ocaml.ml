module Yaks = struct

  include Yaks_common_types

  include Yaks_api_sock.Api

  module Access = Yaks_sock_access.Access
  module Storage = Yaks_sock_storage.Storage

end

