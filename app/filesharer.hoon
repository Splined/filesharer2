/-  *filesharer
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =secret =host =users =public =local =remote]
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ~&  "filesharer compiled successfully!"
  `this
::
++  on-save
  ^-  vase
    !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  ~&  [dap.bowl %load]
  =/  prev  !<(versioned-state old-state)
  ?-  -.prev
      %0
      ~&  >>>  '%0'
      `this(state prev)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %noun
::    ?>  =(our src):bowl
::    ?+    q.vase  (on-poke:def mark vase)
!!
::    ==
      ::
      %filesharer-action
    ~&  >>>  !<(action vase)
    =^  cards  state
    (handle-action:hc !<(action vase))
    [cards this]
  ==
::
::  A ship is added to users when at least one file has that ship in its whitelist
::  So, allow sub for any ship in users.  E.g.  ~(has by users) src.bowl
++  on-watch  on-watch:def
::    |=  =path
::    ^-  (quip card _this)
::    ?.  (~(has by users) src.bowl)
::      ~|("not approved for subscription" !!)
::
::
++  on-leave  on-leave:def
::
++  on-peek  |=(path ~)
::
++  on-agent  |=([wire sign:agent:gall] !!)
::
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
::
::  start helper core
::
|_  bowl=bowl:gall
++  handle-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
        %change-host
    =/  new-host=(unit @t)  (some url.action)
    =.  host  new-host
    `state
        %add-user
    =|  rev=@                               :: start @ 0 and increment?
    =.  users  (~(put by users) ship.action [rev files.action])
    `state
        %remove-user
    =.  users  (~(del by users) ship.action)
    `state
::  *** if perms are included when adding a file to local, need to also add file id
::  *** to files.user for appropriate ships
        %add-file-to-local
    =/  id=@  (mug `@`title.file.action)  :: placeholder for random number gen
    =.  local  (~(put by local) id [file.action perms.action])
::    =/  =cage  [%filesharer-server-update !>([%add-file file.action])]
::    :_  state
::      ~[[%give %fact ~[/updates] cage]]
    `state  :: replace with appropriate card(s)
    ::  if file is removed from local, it should also be removed from all user id lists
        %remove-file-from-local
    =:  local  (~(del by local) id.action)
        users  (~(rut by users) |=([k=ship v=user] `user`[rev.v (~(del in files.v) id.action)]))
    ==
    `state
    ::  add ship if missing from users, then add ship to all wl in set, then add ids to user
        %add-ship-to-wl
    |^
    ?:  (~(has by users) ship.action)
      =:  local  (~(rut by local) add-to-wl)
          users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
      ==
      `state
    =:  users  (~(put by users) ship.action [*rev *(set id)])
        local  (~(rut by local) add-to-wl)
        users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
    ==
    `state
    ::  used with rut:by on 'local' in state to add ship to wl of each file
    ::  in set. e.g. (~(rut by local) add-to-wl)
    ++  add-to-wl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?:  (~(has in ids.action) k)
        :: this returns the value, v, with 'ship' inserted in the 'white' set of the value
        [file.v `perms`[(~(put in white.perms.v) ship.action) black.perms.v]]
      v
::  =. is not the right way to implement this. what should be done instead?
::  just need to return the user with union of existing ids and ids from action
::    ++  add-ids-to-user
::      |=  =user
::      ^-  user
::      =.  user  [rev.user `(set id)`(~(uni in files.user) ids.action)]
    --
        %rm-ship-from-wl
    |^
    =.  local  (~(rut by local) rm-from-wl)
    `state
    ::  used with rut:by on 'local' in state to remove ship from wl of each file
    ::  in set. e.g. (~(rut by local) rm-from-wl)
    ++  rm-from-wl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?:  (~(has in ids.action) k)
        :: this returns the value, v, with 'ship' removed from the 'white' set of the value
        [file.v `perms`[(~(del in white.perms.v) ship.action) black.perms.v]]
      v
    --
        %add-ship-to-bl
    |^
    =.  local  (~(rut by local) add-to-bl)
    `state
    ::  used with rut:by on 'local' in state to add ship to bl of each file
    ::  in set. e.g. (~(rut by local) add-to-bl)
    ++  add-to-bl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?:  (~(has in ids.action) k)
        :: this returns the value, v, with 'ship' inserted in the 'black' set of the value
        [file.v `perms`[white.perms.v (~(put in black.perms.v) ship.action)]]
      v
    --
        %rm-ship-from-bl
    |^
    =.  local  (~(rut by local) rm-from-bl)
    `state
    ::  used with rut:by on 'local' in state to remove ship from bl of each file
    ::  in set. e.g. (~(rut by local) rm-from-bl)
    ++  rm-from-bl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?:  (~(has in ids.action) k)
        :: this returns the value, v, with 'ship' removed from the 'black' set of the value
        [file.v `perms`[white.perms.v (~(del in black.perms.v) ship.action)]]
      v
    --
        %toggle-pub
    ?.  (~(has by local) id.action)
      `state
    ?:  (~(has in public) id.action)
      =.  public  (~(del in public) id.action)
      `state
    =.  public  (~(put in public) id.action)
    `state
        %set-secret
    =.  secret  [k.action iv.action]
    `state
        %encode-test
    ~&  >  (encode [ship.action id.action])
    `state
        %decode-test
    ~&  >  (decode url.action)
    `state
  ==
::
++  encode
  |=  [=ship =id]
  ^-  (unit @t)
  ?~  host  ~
  =/  =user  (~(got by users) ship)
  =/  =file  =<  file  (~(got by local) id)
  =/  raw=@  (can 3 ~[8^id 8^rev.user 16^ship])
  =/  hash=tape
    %-  (v-co:co 32)
    (~(en cbca:aes:crypto k.secret iv.secret) raw)
  =/  query=tape
    ?~  ext.file  ""
    "?ext={(trip u.ext.file)}"
  `(crip "http://{(trip u.host)}/share/{hash}{query}")
::
++  decode
  |=  url=@t
  ^-  (unit decoded)
  =/  purl=(unit purl:eyre)  (de-purl:html url)
  ?~  purl  ~
  =/  =pork:eyre  q.u.purl
  ?.  ?=([@ @ ~] q.pork)  ~
  =/  hash=@  (rash i.t.q.pork vum:ag)
  =/  raw=@  (~(de cbca:aes:crypto k.secret iv.secret) hash)
  :^    ~
      (cut 3 [16 16] raw)
    (cut 3 [8 8] raw)
  (end 6 raw)
::
--
