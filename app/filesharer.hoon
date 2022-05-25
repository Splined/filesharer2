/-  *filesharer
/+  default-agent, dbug, server
/=  webpage  /app/filesharer/index
|%
+$  versioned-state
    $%  state-0
    ==
::
::1  to allow encryption to be toggled on/off add a
::1  ? to state.  e.g. encrypt=?
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
    ?>  =(our src):bowl
    ?+    q.vase  (on-poke:def mark vase)
        %print-subs
      =/  subs=(list ship)
      %+  roll
        ~(val by sup.bowl)
      |=  [v=(pair ship path) ship=(list ship)]
      (snoc ship p.v)
      ~&  >>  subs  `this
        %bind
      %-  (slog leaf+"Attempting to bind /foo." ~)
      :_  this
      [%pass /bind-foo %arvo %e %connect `/'foo' %filesharer]~
::  !!
    ==
      ::
      %filesharer-action
    ~&  >>>  !<(action vase)
    =^  cards  state
    (handle-action:hc !<(action vase))
    [cards this]
::  adapted from gora/sail app
::
      %handle-http-request
    =^  cards  state
    %-  https:hc
    !<([eyre-id=@ta =inbound-request:eyre] vase)
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
        %-  ~(uni in public)
        =>  (~(get by users) who)
          ?~  .  ~
        files.u  
      ~&  >  ids
      =/  local-subset=(map id [file perms])  (local-map-subset ids)
      =/  idata=(map id file)  (~(rut by local-subset) |=([k=@ v=[=file =perms]] file.v))
      ::  encode urls if =(encrypt %.y)
      ::
      =/  data=(map id file)
::1      ?.  encrypt  idata
        %-  ~(rut by idata)
        |=  [k=@ v=file] 
        ^-  file
        :^    title.v
            note.v
          =>  (encode who k)  ?~  .  `@t`~  u
        ext.v
      =/  =cage  [%filesharer-update !>([%init data])]
      ~&  >  cage
      :_  this
        ~[[%give %fact ~ cage]]
        [%http-response *]
      %-  (slog leaf+"Eyre subscribed to {(spud path)}." ~)
      `this
    ==
::
++  on-leave  on-leave:def
::
++  on-peek  ::  |=(path ~)
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    ::  map of all local files and permissions
    ::  .^((list @t) %gx /=filesharer=/local/noun)
    ::
    [%x %local ~]
::      =/  pub-local=^local  (local-map-subset:hc public)
::    ``noun+!>(~(tap by local.state))
    ``noun+!>(local.state)
    ::  set of all public ids
    ::
    ::
    [%x %public ~]
    ``noun+!>(public.state)
    ::  list of all subscribers
    ::  .^((list @p) %gx /=filesharer=/subs/noun)
    ::
    [%x %subs ~]
    =/  subs=(list ship)
    %+  roll
      ~(val by sup.bowl)
    |=  [v=(pair ship ^path) ship=(list ship)]
    (snoc ship p.v)
    ``noun+!>(subs)
    ::  list all files under remote @p
    ::  .^(* %gx /=filesharer=/remote/(scot %p ~sul)/noun)
    ::
    [%x %remote @ta ~]
    =/  uship=(unit @p)  (slaw %p i.t.t.path)
    =/  rship=(map id file)
      (~(got by remote) ?~(uship ~zod u.uship))
::    ``noun+!>(~(tap by rship))
    ``noun+!>(rship)
    ::  list current host
    ::  .^(@t %gx /=filesharer=/host/noun)
    ::
    [%x %host ~]
    ``noun+!>(?~(host *@t u.host))
  ==
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
             %init
           =/  new-remote=(map ship (map id file))  (~(put by remote) src.bowl +.resp)
           [~ this(remote new-remote)]
             %add-remote
           =/  data=(map id file)  +.resp
           =/  new-remote=(map ship (map id file))
             ?.  (~(has by remote) src.bowl)
               (~(put by remote) src.bowl data)
             (~(put by remote) src.bowl (~(uni by data) (~(got by remote) src.bowl)))
           [~ this(remote new-remote)]
             %remove-remote
           =/  data=(set id)  +.resp
           ~&  >  src.bowl                                  :: remove after testing
           ~&  >>  data                                     :: remove after testing
           ::  much improved code for removing 
           ::  files from ~tinnus-napbus
           ::
           =.  remote
             %+  ~(put by remote)
               src.bowl
             %-  ~(rep in data)
             |:  [id=*id files=(~(got by remote) src.bowl)]
             (~(del by files) id)
           `this
         ==
      ==
    ==
  ==
::
++  on-arvo  ::  on-arvo:def
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?.  ?=([%bind-foo ~] wire)
    (on-arvo:def [wire sign-arvo])
  ?>  ?=([%eyre %bound *] sign-arvo)
  ?:  accepted.sign-arvo
    %-  (slog leaf+"/foo bound successfully!" ~)
    `this
  %-  (slog leaf+"Binding /foo failed!" ~)
  `this
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
    =/  file-id=id  (mug `@`title.file.action)  :: placeholder for random number gen or other unique id
    =/  idata=(map id file)  (malt ~[[file-id file.action]])
    |^
    =:  local  (~(put by local) file-id [file.action perms.action])
        users  (~(rut by (update-users white.perms.action)) add-to-users)
    ==
    ::
      :_  state
      (generate-facts idata white.perms.action)
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
    =:  local  (~(del by local) id.action)
        users  (~(rut by users) |=([k=ship v=user] `user`[rev.v (~(del in files.v) id.action)]))
    ==
    =/  =cage  [%filesharer-update !>([%remove-remote ids])]
    :_  state
      ~[[%give %fact (generate-paths ships) cage]] 
        ::  add ship if missing from users, then add ship to all wl in set,
        ::  then add ids to user
        :: 
        %add-ship-to-wl
      =/  idata=(map id file)
      %-  %~  rut
            by
          %-  local-map-subset
          ids.action
      |=  [k=@ v=[=file =perms]]  file.v
      =/  data=(map id file)
::1      ?.  encrypt  idata
        %-  ~(rut by idata)
        |=  [k=@ v=file] 
        ^-  file
        :^    title.v
            note.v
          =>  (encode ship.action k)  ?~  .  `@t`~  u
        ext.v
      =/  =cage  [%filesharer-update !>([%add-remote data])]
    |^
    ?:  (~(has by users) ship.action)
      =:  local  (~(rut by local) add-to-wl)
          users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
      ==
      :_  state
        ~[[%give %fact ~[/updates/(scot %p ship.action)] cage]] 
    =:  users  (~(put by users) ship.action [*rev *(set id)])
        local  (~(rut by local) add-to-wl)
        users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
    ==
    ::
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
::1        :: 
::1        %toggle-encryption
::1    needs to go through every file and change url
        :: 
        %toggle-pub
    =/  slist=(list ship)
    %+  roll
      ~(val by sup.bowl)
    |=  [v=(pair ship path) ship=(list ship)]
    (snoc ship p.v)
    ?.  (~(has by local) id.action)
      `state
    ::  Toggle off.  Delete from only those ships not in file's whitelist
    ::
    ?:  (~(has in public) id.action)
      =.  public  (~(del in public) id.action)
      =/  ids=(set id)  (silt ~[id.action])
      =/  wships=(set ship)  =<  white.perms  (~(got by local) id.action)
      :: need an accumulator for ships in sup.bowl? and not in white.perms
      =/  rships=(set ship)
      %-  ~(rep in wships)
      |:  [ship=*ship subs=(silt slist)]
      (~(del in subs) ship)
      =/  =cage  [%filesharer-update !>([%remove-remote ids])]
      :_  state
      ~[[%give %fact (generate-paths rships) cage]] 
    ::  Toggle on. Send file data to all ships
    ::
    =.  public  (~(put in public) id.action)
    =/  idata=(map id file)
    %-  malt
    :~  :-  id.action
        =<  file  (~(got by local) id.action)
    ==
    :_  state
    (generate-facts idata (silt slist))
::    `state
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
++  https
  |=  [eyre-id=@ta =inbound-request:eyre]
  ?+    method.request.inbound-request
      =/  data=octs
        (as-octs:mimes:html '<h1>405 Method Not Allowed</h1>')
      =/  content-length=@t
        (crip ((d-co:co 1) p.data))
      =/  =response-header:http
        :-  405
        :~  ['Content-Length' content-length]
            ['Content-Type' 'text/html']
            ['Allow' 'GET']
        ==
      :_  state
      :~
        [%give %fact [/http-response/[eyre-id]]~ %http-response-header !>(response-header)]
        [%give %fact [/http-response/[eyre-id]]~ %http-response-data !>(`data)]
        [%give %kick [/http-response/[eyre-id]]~ ~]
      ==
    ::
        %'GET'
      =/  data=octs
    ::    (as-octs:mimes:html '<h1>Hello, World!</h1>')
    ::    test.webpage                      :: works
::        (press.webpage localui.webpage)    :: Internal Server Error
    ::    (manx-to-octs:server localui.webpage)  :: Internal Server Error
          localui-pressed.webpage            :: Internal Server Error
      =/  =response-header:http
        :-  200
        :~    ['Content-Type' 'text/html']
        ==
      :_  state
      :~
        [%give %fact [/http-response/[eyre-id]]~ %http-response-header !>(response-header)]
        [%give %fact [/http-response/[eyre-id]]~ %http-response-data !>(`data)]
        [%give %kick [/http-response/[eyre-id]]~ ~]
      ==
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
::  create subset of local from given ids
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
::  when removing files data will be different for every ship,
::  need to create a seperate path for each.
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
::  when adding files data will be different for every ship
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
::1      ?.  encrypt  idata
      %-  ~(rut by idata)
      |=  [k=@ v=file] 
      ^-  file
      :^    title.v
          note.v
        =>  (encode i.slist k)  ?~  .  `@t`~  u
      ext.v
  ==
--