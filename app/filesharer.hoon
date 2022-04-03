/-  *filesharer
/+  default-agent, dbug
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =secret =host =users =public =local =remote]
::
::  +$  card  card:agent:gall
+$  card  (wind note gift)
+$  gift  gift:agent:gall
+$  note  note:agent:gall
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
++  on-watch  ::  on-watch:def
    |=  =path
    ^-  (quip card _this)
    ?+    path  (on-watch:def path)
        [%updates @ ~]
      ~&  >>>  path
      =/  who=@p  (slav %p i.t.path) 
      =/  ids=(set @)
      ::  handle case of user subscribing without being in any wl
        =>  (~(get by users) who)
          ?~  .  ~
        files.u  
      ~&  >  ids
      =/  local-subset=(map id [file perms])  (local-map-subset ids)
      =/  idata=(map id file)  (~(rut by local-subset) |=([k=@ v=[=file =perms]] file.v))
      ::  encode urls
      ::
      =/  data=(map id file)
        %-  ~(rut by idata)
        |=  [k=@ v=file] 
        ^-  file
        :^    title.v
            note.v
          =>  (encode who k)  ?~  .  `@t`~  u
        ext.v
      =/  =cage  [%filesharer-update !>([%add-remote data])]
      ~&  >  cage
      :_  this
        ~[[%give %fact ~ cage]]
    ==
::
++  on-leave  on-leave:def
::
++  on-peek  |=(path ~)
::
++  on-agent  ::  |=([wire sign:agent:gall] !!)
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+  wire  (on-agent:def wire sign)
    [%updates @ ~]  ::  can I use @p / %p for second element?
    ?+  -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign
        ((slog '%filesharer: Subscribe succeeded!' ~) `this)
      ((slog '%filesharer: Subscribe failed!' ~) `this)
    ::
        %kick
      %-  (slog '%filesharer: Got kick, resubscribing...' ~)
      `this
     :: this created an infinite loop!   :_  this
     :: :~  [%pass /updates/wire %agent [src.bowl %filesharer] %watch /updates]
     :: ==
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %filesharer-update
         =/  resp  !<(update q.cage.sign)
           ~&  >  resp                                      :: remove after testing
         ?-  -.resp
             %add-remote
           =/  data=(map id file)  +.resp
           =/  new-remote=(map ship (map id file))
             ?.  (~(has by remote) src.bowl)
               (~(put by remote) src.bowl data)
             (~(put by remote) src.bowl (~(uni by data) (~(got by remote) src.bowl)))
           [~ this(remote new-remote)]
             %remove-remote
           |^
           =/  data=(set id)  +.resp
           ~&  >  src.bowl                                  :: remove after testing
           ~&  >>  data                                     :: remove after testing
           [~ this(remote (del-ids-in-set data))]
           ::
           ++  del-ids-in-set
             |=  s=(set id)
             ^+  remote
             =/  slist=(list id)  ~(tap in s)
             =/  new-remote=(map ship (map id file))  remote
             |-
             ?~  slist
               ~&  >>>  new-remote
               new-remote
             %=  $
               slist  t.slist
               new-remote  (del-id-by-remote new-remote src.bowl i.slist)
             ==
           ++  del-id-by-remote
             |=  [n=^remote k=ship v=id]
             ^+  remote
             =+  id-map=(~(got by n) k)
             =+  new-map=(~(del by id-map) v)
             ?~  new-map
               (~(del by n) k)
             (~(put by n) k new-map)
           --
         ==
      ==
    ==
  ==
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
        ::    
        %add-user
    =|  rev=@                               :: start @ 0 and increment?
    =.  users  (~(put by users) ship.action *user)  :: how would *file.user be added?
    `state
        :: 
        %remove-user
    |^
      =/  ids=(set id)
        =>  (~(get by users) ship.action)
          ?~  .  ~
        files.u
    =:  local  (~(rut by local) remove-from-wl)
        users  (~(del by users) ship.action)
    ==
    =/  =cage  [%filesharer-update !>([%remove-remote ids])]
    ::  if user is subscribed, they won't be unsubscribed since there are public files
    ::  but send %fact to remove whitelist files from their remote
    :_  state
      ~[[%give %fact ~[/updates/(scot %p ship.action)] cage]] 
    ::
    ++  remove-from-wl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      =/  ids=(set id)                     ::  access this definition from parent?
        =>  (~(get by users) ship.action)
          ?~  .  ~
        files.u
      ?.  (~(has in ids) k)
        v
      :: this returns the value, v, with 'ship' removed from the 'white' set of the value
      [file.v `perms`[(~(del in white.perms.v) ship.action) black.perms.v]]
    --
        ::
        %add-file-to-local
    =/  file-id=id  (mug `@`title.file.action)  :: placeholder for random number gen
    =/  idata=(map id file)  (malt ~[[file-id file.action]])
    |^
    =:  local  (~(put by local) file-id [file.action perms.action])
        users  (~(rut by (update-users white.perms.action)) add-to-users)
    ==
    ::
      :_  state
      (generate-facts idata white.perms.action)
    ::  data will be different for every ship
    ::  need to encode, create cage, and create path for each ship.
    ::
    ++  generate-facts
      |=  [idata=(map id file) ships=(set @p)]
      =|  flist=(list card)
      =/  slist=(list ship)  ~(tap in ships)
      |-  ^-  (list card)
      ?~  slist
        flist
      %=  $
        slist  t.slist
        flist
          %+  snoc
            flist
          :^    %give
              %fact
            ~[/updates/(scot %p i.slist)]
          ^-  cage  :-  %filesharer-update
          !>  :-  %add-remote
          %-  ~(rut by idata)
          |=  [k=@ v=file] 
          ^-  file
          :^    title.v
              note.v
            =>  (encode i.slist k)  ?~  .  `@t`~  u
          ext.v
      ==
    ::  add missing users to state before updating their file lists
    ::
    ++  update-users
      |=  ships=(set ship)
      ^-  (map ship user)
      =/  ship-list=(list ship)  ~(tap in ships)
      =/  missing=(list ship)
        %+  skip
          ship-list
        |=(a=@p (~(has by users) a))
      =/  new-users=(list [ship user])
        (turn missing |=(a=@p [a *user]))
      (~(gas by users) new-users)
    ::  add the whitelisted file ids into each user
    ++  add-to-users
      |=  [k=ship v=user]
      ^-  user
      ?.  (~(has in white.perms.action) k)
        v
      :-  rev.v
      (~(put in files.v) (mug `@`title.file.action))  ::  <-- is there a way to access file-id above?
    --
        ::  if file is removed from local, it should also be removed from all user id lists
        :: 
        %remove-file-from-local
    =/  ids=(set id)  (silt ~[id.action])
    =/  ships=(set ship)  =<  white.perms  (~(got by local) id.action)
    |^
    =:  local  (~(del by local) id.action)
        users  (~(rut by users) |=([k=ship v=user] `user`[rev.v (~(del in files.v) id.action)]))
    ==
    =/  =cage  [%filesharer-update !>([%remove-remote ids])]
    :_  state
      ~[[%give %fact (generate-paths ships) cage]] 
    ::  data will be different for every ship, need to create path for each.
    ::
    ++  generate-paths
      |=  ships=(set @p)
      =|  plist=(list path)
      =/  slist=(list ship)  ~(tap in ships)
      |-  ^-  (list path)
      ?~  slist
        plist
      %=  $
        slist  t.slist
        plist  %+  snoc
                  plist
               /updates/(scot %p i.slist)
      ==
      --
        ::  add ship if missing from users, then add ship to all wl in set,
        ::  then add ids to user
        :: 
        %add-ship-to-wl
    |^
    ?:  (~(has by users) ship.action)
      =:  local  (~(rut by local) add-to-wl)
          users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
      ==
      =/  idata=(map id file)
      %-  %~  rut
            by
          %-  local-map-subset
          ids.action
      |=  [k=@ v=[=file =perms]]  file.v
      =/  data=(map id file)
        %-  ~(rut by idata)
        |=  [k=@ v=file] 
        ^-  file
        :^    title.v
            note.v
          =>  (encode ship.action k)  ?~  .  `@t`~  u
        ext.v
      =/  =cage  [%filesharer-update !>([%add-remote data])]
      :_  state
        ~[[%give %fact ~[/updates/(scot %p ship.action)] cage]] 
    =:  users  (~(put by users) ship.action [*rev *(set id)])
        local  (~(rut by local) add-to-wl)
        users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
    ==
    ::
    =/  idata=(map id file)
    %-  %~  rut
          by
        %-  local-map-subset
        ids.action
    |=  [k=@ v=[=file =perms]]  file.v
    =/  data=(map id file)
       %-  ~(rut by idata)
       |=  [k=@ v=file] 
       ^-  file
       :^    title.v
           note.v
         =>  (encode ship.action k)  ?~  .  `@t`~  u
       ext.v
    =/  =cage  [%filesharer-update !>([%add-remote data])]
    :_  state
      ~[[%give %fact ~[/updates/(scot %p ship.action)] cage]] 
    ::  used with rut:by on 'local' in state to add ship to wl of each file
    ::  in set. e.g. (~(rut by local) add-to-wl)
    ::
    ++  add-to-wl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?.  (~(has in ids.action) k)
        v
      :: this returns the value, v, with 'ship' inserted in the 'white' set of the value
      [file.v `perms`[(~(put in white.perms.v) ship.action) black.perms.v]]
    --
        :: 
        %rm-ship-from-wl
    |^
    =:  local  (~(rut by local) rm-from-wl)
        users  (~(jab by users) ship.action |=(x=user [rev.x (~(dif in files.x) ids.action)]))
    ==
    =/  =cage  [%filesharer-update !>([%remove-remote ids.action])]
    :_  state
      ~[[%give %fact ~[/updates/(scot %p ship.action)] cage]] 
    ::  used with rut:by on 'local' in state to remove ship from wl of each file
    ::  in set. e.g. (~(rut by local) rm-from-wl)
    ++  rm-from-wl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?.  (~(has in ids.action) k)
        v
      :: this returns the value, v, with 'ship' removed from the 'white' set of the value
      [file.v `perms`[(~(del in white.perms.v) ship.action) black.perms.v]]
    --
        :: 
        %add-ship-to-bl
    |^
    =.  local  (~(rut by local) add-to-bl)
    `state
    ::  used with rut:by on 'local' in state to add ship to bl of each file
    ::  in set. e.g. (~(rut by local) add-to-bl)
    ++  add-to-bl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?.  (~(has in ids.action) k)
        v
      :: this returns the value, v, with 'ship' inserted in the 'black' set of the value
      [file.v `perms`[white.perms.v (~(put in black.perms.v) ship.action)]]
    --
        :: 
        %rm-ship-from-bl
    |^
    =.  local  (~(rut by local) rm-from-bl)
    `state
    ::  used with rut:by on 'local' in state to remove ship from bl of each file
    ::  in set. e.g. (~(rut by local) rm-from-bl)
    ++  rm-from-bl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?.  (~(has in ids.action) k)
        v
      :: this returns the value, v, with 'ship' removed from the 'black' set of the value
      [file.v `perms`[white.perms.v (~(del in black.perms.v) ship.action)]]
    --
        :: 
        %toggle-pub
    ?.  (~(has by local) id.action)
      `state
    ?:  (~(has in public) id.action)
      =.  public  (~(del in public) id.action)
      `state
    =.  public  (~(put in public) id.action)
    `state
        :: 
        %set-secret
    =.  secret  [k.action iv.action]
    `state
        :: 
        %encode-test
    ~&  >  (encode [ship.action id.action])
    `state
        :: 
        %decode-test
    ~&  >  (decode url.action)
    `state
        :: 
        %subscribe
::    ~&  >  `path`[host.action ~]
    :_  state
    :~  :*
      %pass   /updates/(scot %p host.action)
      %agent  [host.action %filesharer]
      %watch  /updates/(scot %p our.bowl)
::      %watch  /updates   :: simpler path for testing
    ==  ==
        :: 
        %leave
::    :_  state
    :_  state(remote (~(del by remote) host.action))  ::  is there a way to implement after %leave is confirmed?
    :~  :*
      %pass  /updates/(scot %p host.action)
      %agent  [host.action %filesharer]
      %leave  ~
    ==  ==
    ::
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
++  local-map-subset
  |=  ids=(set id)
  =/  id-list=(list id)  ~(tap in ids)
  =|  sub-local=(map id [file perms])
  |-  ^-  (map id [file perms])
  ?~  id-list
    sub-local
  ?.  (~(has by local) i.id-list)
    $(id-list t.id-list)
  %=  $
    id-list  t.id-list
    sub-local  (~(put by sub-local) i.id-list (~(got by local) i.id-list))
  ==
--
