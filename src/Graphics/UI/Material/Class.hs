module Graphics.UI.Material.Class
  where

newtype Class = Class { unClass :: String }

buildClass :: [Class] -> Class
buildClass = Class . unwords . map unClass

empty = Class ""

mdl_menu = Class "mdl-menu"
mdl_menu_top_right = Class "mdl-menu--top-right"
mdl_js_menu = Class "mdl-js-menu"
mdl_js_ripple_effect = Class "mdl-js-ripple-effect"

mdl_menu_item = Class "mdl-menu__item"

material_icons = Class "material-icons"

mdl_button = Class "mdl-button"
mdl_js_button = Class "mdl-js-button"
mdl_button_icon = Class "mdl-button--icon"

mdl_list = Class "mdl-list"
mdl_list_item = Class "mdl-list__item"
mdl_list_item_primary_content = Class "mdl-list__item-primary-content"
mdl_list_item_secondary_action = Class "mdl-list__item-secondary-action"

mdl_color_text_accent = Class "mdl-color-text--accent"

mdl_switch = Class "mdl-switch"
mdl_js_switch = Class "mdl-js-switch"
mdl_switch_input = Class "mdl-switch__input"
mdl_switch_label = Class "mdl-switch__label"

mdl_slider = Class "mdl-slider"
mdl_js_slider = Class "mdl-js-slider"
