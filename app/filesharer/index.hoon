/-  *filesharer
|_  =bowl:gall
::  ++test works fine when used as test.webpage in main file. ++webui gives 'Internal Server Error' 
++  test  (as-octs:mimes:html '<h1>Hello, World!</h1>')  ::  remove after testing
++  press  (cork en-xml:html as-octt:mimes:html)
++  peek-local-public
  =/  pek=(map id [file perms])  .^((map id [file perms]) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/local/noun)
  =/  pub=(set id)  .^((set id) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/public/noun)
  =/  pub-pek=(map id [file perms])
  %-  ~(rep in pub)
  |:  [id=*id file-perm=pek]
  (~(del by file-perm) id)
  pub-pek
++  peek-local-private
  =/  pek=(map id [file perms])  .^((map id [file perms]) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/local/noun)
  =/  pub=(set id)  .^((set id) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/public/noun)
  =/  pri=(set id)  (~(dif in ~(key by pek)) pub)
  =/  pri-pek=(map id [file perms])
  %-  ~(rep in pri)
  |:  [id=*id file-perm=pek]
  (~(del by file-perm) id)
  pri-pek
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
      ==
    ==
++  peek-remote
  =/  remote=(map ship (map id file))  .^((map ship (map id file)) %gx /(scot %p our.bowl)/filesharer/(scot %da now.bowl)/remotes/noun)
  remote
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
        ;br;
        ;h3: Public files
        ;ul
          ;*  (turn ~(tap by peek-remote) remote-print)
        ==
        ;br;
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
      ==
    ==
--