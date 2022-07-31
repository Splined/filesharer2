::  Todo:
::  -improve update after %POST   ::2
::  -should urls be stored as relative to host? If host is changed,
::  would all urls need to change anyway?
::  Done:
::
::    scrys
::  x  /local          (map id [file perms])     local files
::  x  /public         (set id)                  all public ids
::  x  /subs           (list ship)               all subscribers
::  x  /remote/[ship]  (list file)               files of a ship
::  x  /remotes        (map ship (map id file))  all remote files
::  x  /host           @t                        host server url
::
/-  *filesharer
/+  default-agent, dbug, server
/=  webpage  /app/filesharer/index
|%
+$  versioned-state
    $%  state-0
    ==
::
+$  state-0  [%0 =secret =host =users =public =local =remote =encrypted]
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
  %-  (slog leaf+"Attempting to bind /apps/filesharer." ~)
  =+  [[~ [%apps %filesharer ~]] dap.bowl]
  :_  this
::      [%pass /bind-local %arvo %e %connect `/'filesharer' %filesharer]~
  [%pass /eyre/connect %arvo %e %connect -]~
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
  ?>  =(src.bowl our.bowl)
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
    ==
      ::
      %filesharer-action
    ~&  >>>  !<(action vase)                                ::  remove after testing
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
      ~&  >>>  path                                         ::  remove after testing
      =/  who=@p  (slav %p i.t.path)
      =?  users  ?!  (~(has by users) who)
        (~(put by users) who [*rev *(set id)])
      =/  ids=(set @)
        %-  ~(uni in public)
          =>  (~(got by users) who)  files
      ~&  >  ids                                            ::  remove after testing
      =/  local-subset=(map id [file perms])  (local-map-subset ids)
      =/  idata=(map id file)  (~(rut by local-subset) |=([k=@ v=[=file =perms]] file.v))
      ::  encode urls if =(links:encrypted %.y)
      ::
      =/  data=(map id file)
      ?.  links.encrypted  idata
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
    ::  .^((map id [=file =perms]) %gx /=filesharer=/local/noun)
    ::
    [%x %local ~]
    ``noun+!>(local.state)
    ::  set of all public ids
    ::  .^((set @) %gx /=filesharer=/public/noun)
    ::
    [%x %public ~]
    ``noun+!>(public.state)
    ::  list of all subscribers
    ::  .^((list @p) %gx /=filesharer=/subs/noun)
    ::
    [%x %subs ~]
    =/  subs=(list @p)
    %+  roll
      ~(val by sup.bowl)
    |=  [v=(pair ship ^path) ship=(list ship)]
    (snoc ship p.v)
    ``noun+!>(subs)
    ::  list all files under remote @p
    ::  .^((map id file) %gx /=filesharer=/remote/(scot %p ~sul)/noun)
    ::
    [%x %remote @ta ~]
    =/  uship=(unit @p)  (slaw %p i.t.t.path)
    =/  rship=(map id file)
      (~(got by remote) ?~(uship ~zod u.uship))
    ``noun+!>(rship)
    ::  list all remote @p and files
    ::  .^((map ship (map id file)) %gx /=filesharer=/remote-all/noun)
    ::
    [%x %remotes ~]
    ``noun+!>(remote.state)
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
      :_  this
      :~  [%pass wire %agent [src.bowl %filesharer] %watch /updates]
      ==
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
  ?+  wire  (on-arvo:def [wire sign-arvo])
    [%eyre %connect ~]
  ?>  ?=([%eyre %bound *] sign-arvo)
  ?:  accepted.sign-arvo
    %-  (slog leaf+"/apps/filesharer bound successfully!" ~)
    `this
  %-  (slog leaf+"Binding /apps/filesharer failed!" ~)
  `this
  ==
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
  ::  if host changes, all links should change as well.
  ::  update all url.file in map of local and
  ::  send %init action to all ships in remote
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
      (generate-facts %add-remote idata white.perms.action)
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
    =:  local   (~(del by local) id.action)
        public  (~(del in public) id.action)
        users   (~(rut by users) |=([k=ship v=user] `user`[rev.v (~(del in files.v) id.action)]))
    ==
    =/  =cage  [%filesharer-update !>([%remove-remote ids])]
    :_  state
      ~[[%give %fact (generate-paths ships) cage]] 
    ::  add ship if missing from users, then add ship to all wl in set,
    ::  then add ids to user
    ::
        %add-ship-to-wl
      =?  users  ?!  (~(has by users) ship.action)
        (~(put by users) ship.action [*rev *(set id)])
      =/  idata=(map id file)
      %-  %~  rut
            by
          %-  local-map-subset
          ids.action
      |=  [k=@ v=[=file =perms]]  file.v
      =/  data=(map id file)
      ?.  links.encrypted  idata
        %-  ~(rut by idata)
        |=  [k=@ v=file] 
        ^-  file
        :^    title.v
            note.v
          =>  (encode ship.action k)  ?~  .  `@t`~  u
        ext.v
      =/  =cage  [%filesharer-update !>([%add-remote data])]
    |^
      =:  local  (~(rut by local) add-to-wl)
          users  (~(jab by users) ship.action |=(x=user [rev.x (~(uni in files.x) ids.action)]))
      ==
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
    ::
    ++  rm-from-bl
      |=  [k=id v=[=file =perms]]
      ^-  [file perms]
      ?.  (~(has in ids.action) k)
        v
      :: this returns the value, v, with 'ship' removed from the 'black' set of the value
      [file.v `perms`[white.perms.v (~(del in black.perms.v) ship.action)]]
    --
        ::
        %toggle-encrypted
    =/  slist=(list ship)
    %~  tap
      in
    %-  %~  del
      in
    %-  silt
    %+  roll
      ~(val by sup.bowl)
    |=  [v=(pair ship path) ship=(list ship)]
    (snoc ship p.v)
    our.bowl
    =.  links.encrypted  !links.encrypted
    =/  flist=(list card)
    %+  roll
      slist
    |=  [=ship card=(list card)]
    %+  weld
      card
    =/  ids=(set @)
    %-  ~(uni in public)
    =>  (~(got by users) ship)  files
    =/  local-subset=(map id [file perms])  (local-map-subset ids)
    =/  idata=(map id file)  (~(rut by local-subset) |=([k=@ v=[=file =perms]] file.v))
    ::  if idata is empty a card is still sent to the ship.
    ::  shouldn't cause any issues other that extra traffic
    ::  but probably could be handled better
    (generate-facts %init idata (silt ~[ship]))
    [flist state]
        ::
        %toggle-pub
    =/  slist=(list ship)
    %~  tap
      in
    %-  %~  del
      in
    %-  silt
    %+  roll
      ~(val by sup.bowl)
    |=  [v=(pair ship path) ship=(list ship)]
    (snoc ship p.v)
    our.bowl
    ?.  (~(has by local) id.action)
      `state
    ::  Toggle off.  Delete from only those ships not in file's whitelist
    ::
    ?:  (~(has in public) id.action)
      =.  public  (~(del in public) id.action)
      =/  ids=(set id)  (silt ~[id.action])
      =/  wships=(set ship)  =<  white.perms  (~(got by local) id.action)
      :: ships in sup.bowl and not in white.perms
      ::
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
    (generate-facts %add-remote idata (silt slist))
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
      (four-oh-five eyre-id)
    ::
        %'GET'
      =/  data=octs
      ?+  url.request.inbound-request
          (as-octs:mimes:html '<h1>Hello, World!</h1>')
        %'/apps/filesharer/local'  (press.webpage ~(localui webpage bowl))
        %'/apps/filesharer/remote'  (press.webpage ~(remoteui webpage bowl))
        %'/apps/filesharer/options'  (press.webpage ~(optionsui webpage bowl))
      ==
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
    ::  need to completely reorganize the logic for %'POST's
    ::
    ::
       %'POST'
      ?~  body.request.inbound-request  ~
      =/  query=(map @t @t)
      %-  my
      ^-  (list (pair @t @t))
      =>  (rush q.u.body.request.inbound-request yquy:de-purl:html)
      ?~  .  ~[['' '']]  u       ::  better way to return null?
       ~&  >  query                                         ::  remove after testing
       ~&  >>  (~(get by query) 'fileid')                   ::  remove after testing
      ?~  what=(~(get by query) 'what')
        ~
      ?~  fileid=(~(get by query) 'fileid')
      ?+  u.what
        (four-oh-five eyre-id)
        ::
          %'add'
        =/  =file
        :^    (~(got by query) 'filename')
            (some (~(got by query) 'note'))
          (~(got by query) 'url')
        (some (~(got by query) 'ext'))
        ~&  >>>  file                                       ::  remove after testing
        =|  =perms
        =/  =cage  [%filesharer-action !>([%add-file-to-local file perms])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
        ::
          %'edithost'
        =/  host=@t  (~(got by query) 'hostname')
        =/  =cage  [%filesharer-action !>([%change-host host])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
        ::
          %'editlinks'
        =/  resp=@t  (~(got by query) 'linkname')
        =/  tog=?
          ?|
            ?&
              =(resp 'clear')
              links:encrypted
            ==
            ?&
              =(resp 'hash')
              ?!  links:encrypted
            ==
          ==
        ?.  tog
          `state
        =/  =cage  [%filesharer-action !>([%toggle-encrypted %links])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
        ::
          %'newsub'
        =/  =ship  (slav %p (~(got by query) 'sub'))
        =/  =cage  [%filesharer-action !>([%subscribe ship])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
        ::
          %'leave'
        =/  =ship  (slav %p (~(got by query) 'who'))
        =/  =cage  [%filesharer-action !>([%leave ship])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
      ==
      ~&  >>>  what
      ?+  u.what
        (four-oh-five eyre-id)
        ::
          %'toggle'
        =/  =cage  [%filesharer-action !>([%toggle-pub (slav %ud u.fileid)])]
        :_  state
        ::2  this does update page, but also gives an
        ::2  "unexpected subcription update error in dojo"
        ::2  what is the correct way to update after %POST?
        %+  weld
          ~[[%pass /self %agent [our.bowl dap.bowl] %poke cage]]
        (three-oh-three eyre-id)
        ::
          %'delete'
        =/  =cage  [%filesharer-action !>([%remove-file-from-local (slav %ud u.fileid)])]
        :_  state
        %+  weld
          ~[[%pass /self %agent [our.bowl dap.bowl] %poke cage]]
        (three-oh-three eyre-id)
        ::
          %'add_ship'
        =/  =ship  (slav %p (~(got by query) 'who'))
        =/  ids=(set id)  (silt ~[(slav %ud u.fileid)])
        =/  =cage  [%filesharer-action !>([%add-ship-to-wl ids ship])]
        :_  state
        :~
          [%pass /self %agent [our.bowl dap.bowl] %poke cage]
        ==
        ::
          %'remove'
        =/  =ship  (slav %p (~(got by query) 'who'))
        =/  ids=(set id)  (silt ~[(slav %ud u.fileid)])
        =/  =cage  [%filesharer-action !>([%rm-ship-from-wl ids ship])]
        :_  state
        %+  weld
          ~[[%pass /self %agent [our.bowl dap.bowl] %poke cage]]
        (three-oh-three eyre-id)
      ==
  ==
::
::  No idea if this is correct
++  three-oh-three
  |=  eyre-id=@ta
  ^-  (list card)
  =/  data=octs
    (as-octs:mimes:html '<a href="./local">Success!</a>')
  =/  =response-header:http
    :-  303
    :~    ['Content-Type' 'text/html']
          ['location' './local']
    ==
  :~
    [%give %fact [/http-response/[eyre-id]]~ %http-response-header !>(response-header)]
    [%give %fact [/http-response/[eyre-id]]~ %http-response-data !>(`data)]
    [%give %kick [/http-response/[eyre-id]]~ ~]
  ==
::
++  four-oh-five
  |=  eyre-id=@ta
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
::  ++encode will crash if ship is not in users.
::  However, ships are added to users as needed in ++on-watch and in
::  %add-ship-to-wl, so this should never happen
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
  |=  [mode=$?(%add-remote %init) idata=(map id file) ships=(set @p)]
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
      !>  :-  mode
      ?.  links.encrypted  idata
      %-  ~(rut by idata)
      |=  [k=@ v=file] 
      ^-  file
      :^    title.v
          note.v
        =>  (encode i.slist k)  ?~  .  `@t`~  u
      ext.v
  ==
--
