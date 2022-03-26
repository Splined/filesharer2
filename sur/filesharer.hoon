|%
+$  id  @
+$  file
  $:  title=@t
      note=(unit @t)
      url=@t
      ext=(unit @t)
  ==
+$  perms
  $:  white=(set ship)
      black=(set ship)
  ==
+$  user
  $:  rev=@
      files=(set id)
  ==
+$  decoded
  $:  =ship
      rev=@
      =id
  ==
+$  secret  [k=@ iv=@]
+$  host  (unit @t)
+$  users  (map ship user)
+$  public  (set id)
+$  local  (map id [=file =perms])
+$  remote  (map ship (map id file))
::
::  changes to local data
+$  action
  $%  [%change-host url=@t]
      [%add-user files=(set id) =ship]
      [%remove-user =ship]
      [%add-file-to-local =file =perms]
      [%remove-file-from-local id=@]
      [%add-ship-to-wl ids=(set id) =ship]
      [%rm-ship-from-wl ids=(set id) =ship]
      [%add-ship-to-bl ids=(set id) =ship]
      [%rm-ship-from-bl ids=(set id) =ship]
      [%toggle-pub id=@]
      [%set-secret k=@ iv=@]
      [%encode-test =ship =id]
      [%decode-test url=@t]
      [%subscribe host=@p]   
      [%leave host=@p]
::      [%change-perms        ::  [add|remove] perms for file
  ==
::  changes to remote (i.e. data from other ships)
+$  update
  $%  [%add-remote umap=(map id file)]
::      [%remove-remote =ship]       :: ship info from src.bowl
  ::  [%add-remote =id =file]    :: ship info from src.bowl
:: can be handled by %add-file?      [%add-remote =ship]
:: can be handled by %remove-file?      [%remove-remote =ship]
  ==
--
