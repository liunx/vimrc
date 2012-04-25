" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2008 Dec 17
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set nobackup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" custom settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" gnome-vim settings
" We have to use gvim sometimes, because the vim keys maps may have a 
" conflict with gnome-terminal's key maps, or any other parent shell 
" who launch the vim, so for the seek of use full of keys in vim, we 
" should turn to gnome-vim.(But it seemed to be a little slow)
if has("gui_running")
	set guifont=Monospace\ 12
	set guioptions-=m
	set t_Co=256
	set background=dark
	color wombat256
else
	color delek
endif

set nu

" encoding settings
"
set encoding=utf-8
set fileencodings=utf-8,gb2312,gb18030,gbk,ucs-bom,cp936,latin1 
" 如果你要打开的文件编码不在此列，那就添加进去
set termencoding=utf-8


" set spell checking
"set spell
" set map leader
let mapleader = ","

" set cursor line 
set cursorline

" auto reload our vimrc file
" It's better not to use auto save, or we'll always stuck there
nnoremap <silent> <Leader>rc :so ~/.vimrc<CR>
"autocmd BufWritePost .vimrc source %

" the map to change directory when need
" XXX this will change all buffers' directory
nnoremap <silent> <Leader>cd :lcd %:p:h<CR>:pwd<CR>
" auto change directory
"autocmd BufEnter * lcd %:p:h

" first, enable status line always
set laststatus=2
" set the status line 
set stl=<liunx>\ %f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]
" now set it up to change the status line based on mode
"if version >= 700
"  au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Magenta
"  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
"endif
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=magenta
  elseif a:mode == 'r'
    hi statusline guibg=blue
  else
    hi statusline guibg=red
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=blue

" default the statusline to green when entering Vim
hi statusline guibg=green

" folding settings
"set foldmethod=manual
" set foldenable
highlight Folded guibg=grey guifg=blue
highlight FoldColumn guibg=darkgrey guifg=white

" pop menu settings
highlight Pmenu guibg=black guifg=yellow gui=bold

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Map tab(edit) keys
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent> <Tab>n :tabnew<CR>
nnoremap <silent> <Tab>c :tabclose<CR>
nnoremap <silent> <Tab>o :tabonly<CR>
nnoremap <silent> <Tab>h :tabprevious<CR>
nnoremap <silent> <Tab>l :tabn<CR>
" with this map, we can get steps to move
nnoremap <silent> <Tab>m :execute "tabmove " . input(":")<CR>
nnoremap <silent> <Tab>s :tabs<CR>
nnoremap <silent> <Tab>r :tabrewind<CR>
nnoremap <silent> <Tab>f :tabfirst<CR>
nnoremap <silent> <Tab>e :tablast<CR>
nnoremap <silent> <Tab>d :execute "tabdo " . input(":")<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" omnicppcomplete settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set tags+=~/.vim/tags/cpp
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" python language settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType python setlocal et sta sw=4 sts=4


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" cscope settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if has("cscope")
	set csprg=/usr/bin/cscope
	set csto=0
	set cst
	set nocsverb
	" add any database in current directory
	if filereadable("cscope.out")
	    cs add cscope.out
	" else add database pointed to by environment
	elseif $CSCOPE_DB != ""
	    cs add $CSCOPE_DB
	endif
	set csverb
endif

"set cscopequickfix=s-,c-,d-,i-,t-,e-

map <C-_> :cstag <C-R>=expand("<cword>")<CR><CR>
map g<C-]> :cs find 3 <C-R>=expand("<cword>")<CR><CR>
map g<C-\> :cs find 0 <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-_>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-_>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-_>d :cs find d <C-R>=expand("<cword>")<CR><CR>

" Using 'CTRL-spacebar' then a search type makes the vim window
" split horizontally, with search result displayed in
" the new window.

nmap <C-Space>s :scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>g :scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>c :scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>t :scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>e :scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-Space>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-Space>d :scs find d <C-R>=expand("<cword>")<CR><CR>

" Hitting CTRL-space *twice* before the search type does a vertical
" split instead of a horizontal one

nmap <C-Space><C-Space>s
	\:vert scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>g
	\:vert scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>c
	\:vert scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>t
	\:vert scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>e
	\:vert scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-Space><C-Space>i
	\:vert sc, find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nmap <C-Space><C-Space>d
	\:vert scs find d <C-R>=expand("<cword>")<CR><CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" tags settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent> <Leader>po :pop<CR>
nnoremap <silent> <Leader>ta :tag<CR>
nnoremap <silent> <Leader>ts :tags<CR>
" stjump map, it's quite useful
nnoremap <silent> <Leader>stj :execute "stjump " .  expand("<cword>")<CR>
" we can find tags with this keymap
nnoremap <silent> <Leader>sts :execute "stselect " . input(":")<CR>

set tags+=

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Vim addons manager settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fun SetupVAM()
  let addons_base = expand('$HOME') . '/.vim-addons'
  exec 'set runtimepath+='.addons_base.'/vim-addon-manager'

  " unix based os users may want to use this code checking out VAM
  " if !isdirectory(addons_base)
  "   exec '!p='.shellescape(addons_base).'; mkdir -p "$p" && cd "$p" && git clone git://github.com/MarcWeber/vim-addon-manager.git'
  " endif

  " commenting try .. endtry because trace is lost if you use it.
  " There should be no exception anyway
  " try
    call vam#ActivateAddons(['taglist', 'c%213', 'The_NERD_tree', 'FuzzyFinder', 'session%3150', 'Conque_Shell', 'Mark%2666', 'fugitive', 'VisIncr', 'dbext', 'verilog_systemverilog', 'verilog', 'perl-support', 'bash-support', 'Vim-Support', 'perlomni', 'perlhelp', 'pythoncomplete', 'Pydiction', 'pydoc%910', 'CCTree', 'xml', 'The_NERD_Commenter', 'simple_bookmarks', 'ccvext', 'Visual_Mark', 'YankRing', 'colorselector', 'moria', 'hexman', 'Powerline', 'current-func-info', 'StatusLineHighlight', 'vcscommand', 'AutoComplPop', 'simplefold'], {'auto_install' : 0})
"    call vam#ActivateAddons(['taglist', 'c%213', 'The_NERD_tree', 'FuzzyFinder', 'session%3150', 'Conque_Shell', 'Mark%2666', 'fugitive', 'VisIncr', 'dbext', 'verilog_systemverilog', 'verilog', 'perl-support', 'bash-support', 'Vim-Support', 'AutoComplPop', 'perlomni', 'perlhelp', 'pythoncomplete', 'Pydiction', 'pydoc%910', 'CCTree', 'xml', 'The_NERD_Commenter', 'simple_bookmarks', 'ccvext', 'Visual_Mark', 'YankRing', 'colorselector', 'vimbuddy%8', 'ifdef_highlighting', 'echofunc', 'moria', 'hexman', 'statusline', 'Powerline', 'current-func-info', 'StatusLineHighlight', 'AutoFold', 'vcscommand'], {'auto_install' : 0})
    " pluginA could be github:YourName see vam#install#RewriteName()
  " catch /.*/
  "  echoe v:exception
  " endtry
endf
call SetupVAM()
" experimental: run after gui has been started (gvim) [3]
" option1:  au VimEnter * call SetupVAM()
" option2:  au GUIEnter * call SetupVAM()
" See BUGS sections below [*]


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" csupport settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" point out the local templates 
"let g:C_LocalTemplateFile = expand('$HOME') . '/.vim-addons/c.vim_-_CC_IDE/c-support/templates/Templates'


" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" TagList settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent><Leader>tlt :TlistToggle<CR>
nnoremap <silent><Leader>tlu :TlistUpdate<CR>
" close the taglist window when only the taglist window is present
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_SingleClick = 1
let Tlist_Show_One_File = 1
let Tlist_Inc_Winwidth = 0
let Tlist_Process_File_Always = 1
let Tlist_Use_Right_Window = 1
let Tlist_Auto_Update = 1
let Tlist_GainFocus_On_ToggleOpen = 1

"XXX do not set the statusline output, or we can not see the ruler
"set statusline=%<%f%=%([%{Tlist_Get_Tagname_By_Line()}]%)
set title titlestring=%<%f\ %([%{Tlist_Get_Tagname_By_Line()}]%)
"let Tlist_Show_Menu = 1

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" NERDTree settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent><Leader>ntt :NERDTreeToggle<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" fuzzy finder settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 400
let g:fuf_mrucmd_maxItem = 400
let g:fuf_help_cache_dir = ''
let g:fuf_tag_cache_dir = ''
let g:fuf_taggedfile_cache_dir = ''
" this function is a proxy to execute fuzzer finder commands
function! ExFuzzyFinder()
	let input = input(":")
	if (input == "fb") 
		execute "FufBuffer"
	elseif (input == "fcbd") 
		execute "FufFileWithCurrentBufferDir"
	elseif (input == "ffc") 
		execute "FufFileWithFullCwd"
	elseif (input == "ff") 
		execute "FufFile"
	elseif (input == "fcfc") 
		execute "FufCoverageFileChange"
	elseif (input == "fcfr") 
		execute "FufCoverageFileRegister"
	elseif (input == "fdcbd") 
		execute "FufDirWithCurrentBufferDir"
	elseif (input == "fdfc") 
		execute "FufDirWithFullCwd"
	elseif (input == "fd") 
		execute "FufDir"
	elseif (input == "fmf") 
		execute "FufMruFile"
	elseif (input == "fmfc") 
		execute "FufMruFileInCwd"
	elseif (input == "fmc") 
		execute "FufMruCmd"
	elseif (input == "fbf") 
		execute "FufBookmarkFile"
	elseif (input == "fbfa") 
		execute "FufBookmarkFileAdd"
	elseif (input == "fbfast") 
		execute "FufBookmarkFileAddAsSelectedText"
	elseif (input == "fbd") 
		execute "FufBookmarkDir"
	elseif (input == "fbda") 
		execute "FufBookmarkDirAdd"
	elseif (input == "ft") 
		execute "FufTag"
	elseif (input == "ftn") 
		execute "FufTag!"
	elseif (input == "ftcw") 
		execute "FufTagWithCursorWord!"
	elseif (input == "fbt") 
		execute "FufBufferTag"
	elseif (input == "fbtn") 
		execute "FufBufferTag!"
	elseif (input == "fbtstn") 
		execute "FufBufferTagWithSelectedText!"
	elseif (input == "fbtst") 
		execute "FufBufferTagWithSelectedText"
	elseif (input == "fbtcwn") 
		execute "FufBufferTagWithCursorWord!"
	elseif (input == "fbta") 
		execute "FufBufferTagAll"
	elseif (input == "fbtan") 
		execute "FufBufferTagAll!"
	elseif (input == "fbtastn") 
		execute "FufBufferTagAllWithSelectedText!"
	elseif (input == "fbtast") 
		execute "FufBufferTagAllWithSelectedText"
	elseif (input == "fbtwcwn") 
		execute "FufBufferTagAllWithCursorWord!"
	elseif (input == "ftf") 
		execute "FufTaggedFile"
	elseif (input == "ftfn") 
		execute "FufTaggedFile!"
	elseif (input == "fjl") 
		execute "FufJumpList"
	elseif (input == "fcl") 
		execute "FufChangeList"
	elseif (input == "fqf") 
		execute "FufQuickfix"
	elseif (input == "fl") 
		execute "FufLine"
	elseif (input == "fh") 
		execute "FufHelp"
	elseif (input == "fedf") 
		execute "FufEditDataFile"
	elseif (input == "frc") 
		execute "FufRenewCache"
	endif
endfunction

" map this function
nnoremap <silent> <Leader>ff	:call ExFuzzyFinder()<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" sessionman settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"nnoremap <silent><C-s>o :execute "SessionOpen" . input(":")<CR>
"nnoremap <silent><C-s>s :SessionSave<CR>
"nnoremap <silent><C-s>a :SessionSaveAs<CR>
"nnoremap <silent><C-s>c :SessionClose<CR>
"nnoremap <silent><C-s>l :SessionList<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" ConqueTerm settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Python version
"let g:ConqueTerm_PyVersion = 2
" Color Support
let g:ConqueTerm_Color = 1
" Session Support
let g:ConqueTerm_SessionSupport = 1
" Keep updating terminal buffer 
let g:ConqueTerm_ReadUnfocused = 1
nnoremap <silent> <Leader>ctb :ConqueTerm bash<CR>
nnoremap <silent> <Leader>cts :ConqueTermSplit bash<CR>
nnoremap <silent> <Leader>ctv :ConqueTermVSplit bash<CR>
nnoremap <silent> <Leader>ctt :ConqueTermTab bash<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" dbext settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"let g:dbext_default_profile_201 = 'type=MYSQL:user=root:passwd=123456:extra=--default-character-set=utf8:dbname=enterprise:host=192.168.1.201:port=3306'
"let g:dbext_default_profile_76 = 'type=MYSQL:user=enet:passwd=123456:extra=--default-character-set=utf8:dbname=public:host=218.29.188.76:port=3306'
"let g:dbext_default_profile_52 = 'type=MYSQL:user=root:passwd=root@mysql@218.29.188.52@enet:extra=--default-character-set=utf8:dbname=enet:host=218.29.188.52:port=3306'


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" search for visually selected text
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" you can yank the hightlighted text first. then
" /
" Ctrl r
" "

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" folder settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set fdm=syntax
highlight Folded guibg=Black guifg=DarkGray

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Mark settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set viminfo+=!  " Save and restore global variables. 


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" session by Peter Odding settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:session_autosave = 'yes'
let g:session_autoload = 'no'

" Add list sessions function.
if has("win32") || has("dos32") || has("dos16") || has("os2")
	let s:sessions_path = ($HOME != '') ? $HOME . '/vimfiles' : ($APPDATA != '') ? $APPDATA . '/Vim' : $VIM
	let s:sessions_path = substitute(s:sessions_path, '\\', '/', 'g') . '/sessions'
else
	let s:sessions_path = $HOME . '/.vim/sessions'
endif

function! s:MyListSessions()
	let w_sl = bufwinnr("__SessionList__")
	if w_sl != -1
		execute w_sl . 'wincmd w'
		return
	endif
	silent! split __SessionList__

	" Mark the buffer as scratch
	setlocal buftype=nofile
	setlocal bufhidden=wipe
	setlocal noswapfile
	setlocal nowrap
	setlocal nobuflisted

	nnoremap <buffer> <silent> q :bwipeout!<CR>
 	nnoremap <buffer> <silent> o :execute "OpenSession " . getline('.')<CR>
 	nnoremap <buffer> <silent> <CR> :execute "OpenSession " . getline('.')<CR>
 	nnoremap <buffer> <silent> <2-LeftMouse> :execute "OpenSession " . getline('.')<CR>
 	nnoremap <buffer> <silent> d :execute "DeleteSession " . getline('.')<CR>
 	nnoremap <buffer> <silent> e :execute "ViewSession" . getline('.')<CR>

	syn match Comment "^\".*"
	put ='\"-----------------------------------------------------'
	put ='\" q                        - close session list'
	put ='\" o, <CR>, <2-LeftMouse>   - open session'
	put ='\" d                        - delete session'
	put ='\" e                        - edit session'
	put ='\"-----------------------------------------------------'
	put =''
	let l = line(".")

	let sessions = substitute(glob(s:sessions_path . '/*.vim'), '\\', '/', 'g')
	let sessions = substitute(sessions, "\\(^\\|\n\\)" . s:sessions_path . '/', '\1', 'g')
	let sessions = substitute(sessions, "\n[^\n]\\+x\\.vim\n", '\n', 'g')
    let sessions = substitute(sessions, ".vim", '', 'g')
	if sessions == ''
		syn match Error "^\" There.*"
		let sessions = '" There are no saved sessions'
	endif
	silent put =sessions

	0,1d
 	execute l
	setlocal nomodifiable
	setlocal nospell
endfunction
command! -nargs=0 ListSessions call s:MyListSessions()

nnoremap <silent><C-s>o :execute "OpenSession " . input(":")<CR>
nnoremap <silent><C-s>s :execute "SaveSession " . input(":")<CR>
nnoremap <silent><C-s>c :execute "CloseSession " . input(":")<CR>
nnoremap <silent><C-s>d :execute "DeleteSession " . input(":")<CR>
nnoremap <silent><C-s>v :execute "ViewSession " . input(":")<CR>
nnoremap <silent><C-s>r :execute "RestartVim " . input(":")<CR>
nnoremap <silent><C-s>l :execute "ListSessions "<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" simple_bookmarks settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent><C-m>b :execute "Bookmark " . input(":")<CR>
nnoremap <silent><C-m>g :execute "GotoBookmark " . input(":")<CR>
nnoremap <silent><C-m>d :execute "DelBookmark " . input(":")<CR>
nnoremap <silent><C-m>o :execute "CopenBookmarks"<CR>

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" YankRing settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:yankring_history_dir = '$HOME/.vim'
nnoremap <silent> <C-y>s :YRShow<CR>
nnoremap <silent> <C-y>t :YRToggle<CR>
nnoremap <silent> <C-y>c :YRClear<CR>
nnoremap <silent> <C-y>g :YRGetElem<CR>
nnoremap <silent> <C-y>r :YRSearch<CR>
nnoremap <silent> <C-y>m :execute "YRGetMultiple " . input(":")<CR>
nnoremap <silent> <C-y>p :execute "YRPush '" . input(":") . "'"<CR>
nnoremap <silent> <C-y>o :execute "YRPop " . input(":")<CR>

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" SelectColorS settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" M-x + commands may be a good way for some command
" that not be always used.

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Powerline settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" let g:Powerline_dividers_override = ['>>', '>', '<<', '<']

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" FileType indent settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType vim set tabstop=4 shiftwidth=4 expandtab
