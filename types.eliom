{shared{

type timestamp = float deriving (Json)
type id = int deriving (Json)

type event = {
  e_timestamp: timestamp;
  e_desc:  string;
  e_title: string;
  e_url:   string;
  e_shortcut: string;  (* UNIQUE *)
}

}}
