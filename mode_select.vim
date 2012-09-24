"#################################################################################
"
"       Filename:  mode_select.vim
"
"    Description:  We can use the plugin to switch among different language 
"                  indent mode
"
"   GVIM Version:  7.0+
"
"  Configuration:  
"
"         Author:  Lei Liu
"          Email:  liunx163@163.com
"
"        Version:  see variable  g:ModeSelect_Version  below
"        Created:  07.08.2012
"        License:  Copyright (c) 2000-2011, Lei Liu
"                  This program is free software; you can redistribute it and/or
"                  modify it under the terms of the GNU General Public License as
"                  published by the Free Software Foundation, version 2 of the
"                  License.
"                  This program is distributed in the hope that it will be
"                  useful, but WITHOUT ANY WARRANTY; without even the implied
"                  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
"                  PURPOSE.
"                  See the GNU General Public License version 2 for more details.
"       Revision:  $Id: c.vim,v 1.156 2011/12/27 21:05:07 mehner Exp $
"
"------------------------------------------------------------------------------
"
if v:version < 700
  echohl WarningMsg | echo 'The plugin mode_select.vim needs Vim version >= 7 .'| echohl None
  finish
endif
"
" Prevent duplicate loading:
"
if exists("g:ModeSelect_Version") || &cp
 finish
endif
let g:C_Version= "0.0.1"  							" version number of this script; do not change

"
"------------------------------------------------------------------------------
"
" select different mode 
"
function! ModeSelect()
	let s:input = input(":")
    if (s:input == "bsd")
        set et sta sw=4 sts=4
    elseif (s:input == "bcm")
        set et sta sw=3 sts=3
    elseif (s:input == "linux")
        set tabstop=8 softtabstop=8 shiftwidth=8 noexpandtab
    endif
endfunction

command ModeSelect execute "call ModeSelect()"
