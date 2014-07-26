{shared{

type timestamp = int deriving (Json)

type event = {
  e_timestamp: timestamp;
  e_desc:  string;
  e_title: string;
  e_url:   string;
}

}}
