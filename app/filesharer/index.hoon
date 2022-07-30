::  Todo:
::  Done:
::  -add list of subscribers to local? page
::  -add forms for link and http encryption to options page
::
/-  *filesharer
|_  =bowl:gall
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
++  peek-subs
  ^-  (list @p)
  %~  tap
    in
  %-  %~  del
    in
  %-  silt
  .^((list @p) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/subs/noun)
  our.bowl
++  sub-print
  |=  =ship
  ;li
    ; {(scow %p ship)}
  ==
++  local-print
  |=  [a=id b=(pair file perms)]
  =/  note  ?~(note.p.b *@t u.note.p.b)
  =/  ext  ?~(ext.p.b *@t u.ext.p.b)
  ;li
    ; {(trip title.p.b)}
    ;form(method "post")
      ;input(type "submit", name "what", value "delete");
      ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
    ==
    ;ul
      ;li: {(trip note)}
      ;li
        ;a(href <(trip url.p.b)>): {(trip url.p.b)}
      ==
::      ;li: {(trip url.p.b)}
      ;li: {(trip ext)}
    ==
    ;form(method "post")
      ;input(type "submit", name "what", value "toggle");
      ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
    ==
    :: form to add ship to whitelist
    ::
    ;br;
    ; Whitelisted ships
    ;form(method "post")
      ;input(type "submit", name "what", value "add_ship");
      ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
      ;input(type "text", name "who", placeholder "~sampel");
      ;br;
    ==
    :: set of wl ships and a form for each to remove
    ::
    ;ul
      ;*  ^-  marl
          %+  turn  ~(tap in white.q.b)
          |=  w=ship
          ;li
              ; {(scow %p w)}
              ;form(method "post")
                ;input(type "submit", name "what", value "remove");
                ;input(type "hidden", name "fileid", value "{(scow %ud a)}");
                ;input(type "hidden", name "who", value "{(scow %p w)}");
              ==
          ==
    ==
    ;br;
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
          ;label(for "filename"): New file:
          ;br;
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
        ;h3: Subscribers
        ;ul
          ;*  (turn peek-subs sub-print)
        ==
        ;br;
        ;a/"./remote": Remote files
        ;br;
        ;a/"./options": Edit program options
      ==
    ==
++  peek-remote
::  .^((map ship (map id file)) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/remotes/noun)
  .^  (map ship (map id file))
    %gx
    /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/remotes/noun
  ==
++  remote-print
  |=  [a=ship b=(map id file)]
  ;li
    ; {(scow %p a)}
    ;form(method "post")
      ;input(type "submit", name "what", value "leave");
      ;input(type "hidden", name "who", value "{(scow %p a)}");
    ==
    ;ul
      ;*  %+  turn  ~(tap by b)
          |=  (pair id file)
          =/  note  ?~(note.q *@t u.note.q)
          =/  ext  ?~(ext.q *@t u.ext.q)
          ;li
            ; {(trip title.q)}
            ;ul
              ;li: {(trip note)}
              ;li
                ;a(href <(trip url.q)>): {(trip url.q)}
              ==
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
        ;form(method "post")
::          ;label(for "hostname"):Change host:
::          ;label: New sub:
          ;input(type "submit", name "what", value "newsub");
          ;input(type "text", name "sub", placeholder "~sampel");
          ;br;
        ==
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
        ;form(method "post")
::          ;h4: New file
::          ;label(for "hostname"):Change host:
         ;label(for "httpselect"): http type:
         ;select(name "httpname", id "httpselect")
           ;option(value "http"): http
           ;option(value "https"): https
         ==
          ;label: Change host:
          ;input(type "text", name "hostname", placeholder "hostname.com");
          ;br;
          ;input(type "submit", name "what", value "edithost");
        ==
        ;br;
        ;form(method "post")
         ;label(for "linkselect"): link type:
         ;select(name "linkname", id "linkselect")
           ;option(value "clear"): clear
           ;option(value "hash"): encoded
         ==
         ;br;
         ;input(type "submit", name "what", value "editlinks");
        ==
        ;br;
        ;a/"./local": Local files
        ;br;
        ;a/"./remote": Remote files
      ==
    ==
--
