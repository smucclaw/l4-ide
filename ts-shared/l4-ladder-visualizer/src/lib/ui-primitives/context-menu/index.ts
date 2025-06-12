import { ContextMenu as ContextMenuPrimitive } from 'bits-ui'

import Trigger from './context-menu-trigger.svelte'
import Item from './context-menu-item.svelte'
import Content from './context-menu-content.svelte'
import Shortcut from './context-menu-shortcut.svelte'
import Separator from './context-menu-separator.svelte'
import SubContent from './context-menu-sub-content.svelte'
import SubTrigger from './context-menu-sub-trigger.svelte'
import CheckboxItem from './context-menu-checkbox-item.svelte'
import Label from './context-menu-label.svelte'
const Sub = ContextMenuPrimitive.Sub
const Root = ContextMenuPrimitive.Root

export {
  Sub,
  Root,
  Item,
  Label,
  Trigger,
  Content,
  Shortcut,
  Separator,
  SubContent,
  SubTrigger,
  CheckboxItem,
  //
  Root as ContextMenu,
  Sub as ContextMenuSub,
  Item as ContextMenuItem,
  Content as ContextMenuContent,
  Trigger as ContextMenuTrigger,
  Shortcut as ContextMenuShortcut,
  Separator as ContextMenuSeparator,
  SubContent as ContextMenuSubContent,
  SubTrigger as ContextMenuSubTrigger,
  CheckboxItem as ContextMenuCheckboxItem,
  Label as ContextMenuLabel,
}
