/-  *filesharer
|_  =bowl:gall
::  ++test works fine when used as test.webpage in main file. ++webui gives 'Internal Server Error' 
++  test  (as-octs:mimes:html '<h1>Hello, World!</h1>')  ::  remove after testing
++  press  (cork en-xml:html as-octt:mimes:html)
++  peek-local-private
  =/  pek=(map id [file perms])  .^((map id [file perms]) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/local/noun)
  =/  pub=(set id)  .^((set id) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/public/noun)
  =/  pub-pek=(map id [file perms])
  %-  ~(rep in pub)
  |:  [id=*id file-perm=pek]
  (~(del by file-perm) id)
  pub-pek
++  peek-local-public
  =/  pek=(map id [file perms])  .^((map id [file perms]) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/local/noun)
  =/  pub=(set id)  .^((set id) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/public/noun)
  =/  pri=(set id)  (~(dif in ~(key by pek)) pub)
  =/  pri-pek=(map id [file perms])
  %-  ~(rep in pri)
  |:  [id=*id file-perm=pek]
  (~(del by file-perm) id)
  pri-pek
::
++  friend-adder
|=  =ship
^-  manx
;form(method "post")
  ;input(type "hidden", name "who", value "{(scow %p ship)}");
  ;button(type "submit", name "what", value "meet"):"+"
==
::
++  local-print
  |=  [a=id b=(pair file perms)]
  =/  note  ?~(note.p.b *@t u.note.p.b)
  =/  ext  ?~(ext.p.b *@t u.ext.p.b)
  ;li
    ; {(trip title.p.b)}
    ;ul
      ;li: {(trip note)}
      ;li: {(trip url.p.b)}
      ;li: {(trip ext)}
    ==
    ;form(method "post")
      ;input(type "submit", name "what", value "toggle");
      ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
    ==
    ;form(method "post")
      ;input(type "submit", name "what", value "remove");
      ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
    ==
  ==
++  localui
    ^-  manx
    ;html
      ;head
        ;title:"local files"
        ;style:"form \{ display: inline-block; }"
        ;meta(charset "utf-8");
        ;meta(name "viewport", content "width=device-width, initial-scale=1");
      ==
      ;body
        ::
        ;h1: Local files
        ;br;
        ;form(method "post")
          ;h4: New file
          ;input(type "text", name "filename", placeholder "filename");
          ;br;
          ;input(type "text", name "note", placeholder "note");
          ;br;
          ;input(type "text", name "url", placeholder "http://path.com");
          ;br;
          ;input(type "text", name "ext", placeholder "txt,jpg,etc");
          ;br;
          ;input(type "submit", name "what", value "add");
        ==
        ;br;
        ;h3: Public files
        ;ul
          ;*  (turn ~(tap by peek-local-public) local-print)
        ==
        ;br;
        ;h3: Private files
        ;ul
          ;*  (turn ~(tap by peek-local-private) local-print)
        ==
        ;br;
        ;a/"./remote": Remote files
        ;br;
        ;a/"./options": Edit program options
      ==
    ==
++  peek-remote
  .^((map ship (map id file)) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/remotes/noun)
::  =/  remote=(map ship (map id file))  .^((map ship (map id file)) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/remotes/noun)
::  remote
++  remote-print
  |=  [a=ship b=(map id file)]
  ;li
    ; {(scow %p a)}
    ;ul
      ;*  %+  turn  ~(tap by b)
          |=  (pair id file)
          =/  note  ?~(note.q *@t u.note.q)
          =/  ext  ?~(ext.q *@t u.ext.q)
          ;li
            ; {(trip title.q)}
            ;ul
              ;li: {(trip note)}
              ;li: {(trip url.q)}
              ;li: {(trip ext)}
            ==
          == 
    ==
  ==
++  remoteui
    ^-  manx
    ;html
      ;head
        ;title:"remote files"
        ;style:"form \{ display: inline-block; }"
        ;meta(charset "utf-8");
        ;meta(name "viewport", content "width=device-width, initial-scale=1");
      ==
      ;body
        ::
        ;h1: Remote files
        ;ul
          ;*  (turn ~(tap by peek-remote) remote-print)
        ==
        ;br;
        ;br;
        ;a/"./local": Local files
        ;br;
        ;a/"./options": Edit program options
      ==
    ==
++  optionsui
    ^-  manx
    ;html
      ;head
        ;title:"remote files"
        ;style:"form \{ display: inline-block; }"
        ;meta(charset "utf-8");
        ;meta(name "viewport", content "width=device-width, initial-scale=1");
      ==
      ;body
        ::
        ;h1: Program settings
        ;h3: Host server
        :: is there a better way to display scry results?
        :: (trip .^(@t ...)) just displays scry text itself
        ::
        {<.^(@t %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/host/noun)>}
        ;br;
        ;b: Links encrypted?
        ;br;
        ;a/"./local": Local files
        ;br;
        ;a/"./remote": Remote files
      ==
    ==
--
