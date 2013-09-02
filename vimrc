" ===========================================================================
"                               liunx's VIMRC
" Author:       Lei Liu <liu163@163.com>
" 
" Instruction:  This vimrc file is used for c/c++ and many other script based
"               languages like perl, python, javascript etc.
" ===========================================================================

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" vim based settings
" these settings just care about the vim-self based settings.
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  set backup		" keep a backup file
endif

" we should point out a backupdir rather that let vim write backup file
" in your current edit area, that's make thing mess.
if !isdirectory($HOME . "/.vimbackup")
    call mkdir($HOME . "/.vimbackup")
endif
set backupdir=~/.vimbackup
let &directory = &backupdir

" undo directory and file settings
if has('persistent_undo')
    if !isdirectory($HOME . "/.vimundo")
        call mkdir($HOME . "/.vimundo")
    endif
    set undodir=~/.vimundo
    set undofile
endif
" do not give the intro when you type :intro
set shortmess+=I
" set the command line below the statusline to just 1, that's enough for us.
set cmdheight=1

" A hidden buffer is a buffer with some unsaved modifications and is not 
" displayed in a window. Hidden buffers are useful, if you want to edit
" multiple buffers without saving the modifications made to a buffer while 
" loading other buffers. 
set hidden
set history=256		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" diff mode settings
set diffopt=filler,icase,iwhite

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

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Vim addons manager settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fun SetupVAM()
  let addons_base = expand('$HOME') . '/.vim-addons'
  exec 'set runtimepath+='.addons_base.'/vim-addon-manager'

  " unix based os users may want to use this code checking out VAM
  if !isdirectory(addons_base)
    exec '!p='.shellescape(addons_base).'; mkdir -p "$p" && cd "$p" && git clone git://github.com/MarcWeber/vim-addon-manager.git'
  endif

  " commenting try .. endtry because trace is lost if you use it.
  " There should be no exception anyway
  " try
    call vam#ActivateAddons(['c%213', 'The_NERD_tree', 'FuzzyFinder', 'session%3150', 'Mark%2666', 'VisIncr', 'perl-support', 'bash-support', 'Vim-Support', 'The_NERD_Commenter', 'simple_bookmarks', 'YankRing', 'colorselector', 'hexman', 'powerline', 'current-func-info', 'DoxygenToolkit', 'tlib', 'TxtBrowser', 'JavaScript_syntax', 'smartword', 'grep', 'undotree', 'calendar%52', 'Tagbar', 'colorsel', 'SudoEdit', 'Vimarok', 'Colour_Sampler_Pack', 'lua-support', 'xml', 'neocomplete', 'tslime', 'vim-multiple-cursors'], {'auto_install' : 1})
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


" gnome-vim settings
" We have to use gvim sometimes, because the vim keys maps may have a 
" conflict with gnome-terminal's key maps, or any other parent shell 
" who launch the vim, so for the seek of use full of keys in vim, we 
" should turn to gnome-vim.(But it seemed to be a little slow)
if has("gui_running")
	set guifont=Monospace\ 12
	set guioptions-=m
    " do not show tool bars
    set guioptions-=T
    set guioptions+=LlRrb
    set guioptions-=LlRrb
	set t_Co=256
	set background=dark
	colorscheme jellybeans
else
	color delek
endif

" we may prefer relativenumber that number
if exists('+relativenumber')
"    set relativenumber
    set number
else
set number
endif

" XXX encoding settings XXX
"
set encoding=utf-8
set fileencodings=utf-8,gb2312,gb18030,gbk,ucs-bom,cp936,latin1 
" 如果你要打开的文件编码不在此列，那就添加进去
set termencoding=utf-8

" XXX the mapleader i prefer ","
let mapleader = ","

" XXX searching settings
set magic
set ignorecase
set smartcase
set hlsearch
set incsearch

" Highlight the screen line of the cursor with CursorLine
set cursorline
set cursorcolumn
hi CursorLine ctermbg=235 cterm=None
hi CursorColumn ctermbg=235

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
set wildmode=list:full
" the wildmenu will highlight what you chose in the status line.
set wildmenu
set scrolloff=0

" By default whitespace will be hidden, but now it can be toggled with ,s.
" it's useful for us to see whether lines are aligned
set listchars=tab:--,trail:·,eol:$
nmap <silent> <leader>s :set nolist!<CR>

"uswitch among e.g. if/elsif/else/end, between opening and closing XML tags, and more
" matchit has been included in vim distribution
runtime macros/matchit.vim

" set the status line 
" XXX we'll use powerline plugin instead
"set statusline=<liunx>\ %f\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

" folding settings
"set foldmethod=manual
" set foldenable
"highlight Folded guibg=grey guifg=blue
"highlight FoldColumn guibg=darkgrey guifg=white

" pop menu settings
"highlight Pmenu guibg=black guifg=yellow gui=bold

" detail please press ":help completeopt"
set completeopt=menu,preview,menuone
" XXX isfname
set isfname-=-
set complete=.,w,b,u,t,i

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

" XXX it seems that it's just work in terminal
highlight TabLine term=reverse cterm=reverse ctermfg=white ctermbg=black
highlight TabLineSel term=bold cterm=bold,underline ctermfg=5
highlight TabLineFill term=reverse cterm=reverse ctermfg=white ctermbg=black

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" omnicppcomplete settings XXX DO WE NEED IT ANY MORE? XXX
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"set tags+=~/.vim/tags/cpp
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

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" python language settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType python setlocal et sta sw=4 sts=4
autocmd FileType python setlocal foldmethod=indent

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" tcl language settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType tcl setlocal et sta sw=4 sts=4

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" lua language settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType lua setlocal et sta sw=4 sts=4
autocmd FileType lua setlocal foldmethod=indent

" below are custom settings for lua-support
let g:Lua_AuthorName  = 'Lei Liu'
let g:Lua_AuthorRef   = 'liunx'
let g:Lua_Company     = 'Newrocktech'
let g:Lua_Email       = 'liunx163@163.com'

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

" =============================================================================
"                  XXX  SETTINGS FOR PLUGINS  XXX
"
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" csupport settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" point out the local templates 
"let g:C_LocalTemplateFile = expand('$HOME') . '/.vim-addons/c.vim_-_CC_IDE/c-support/templates/Templates'

" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Tagbar settings
"   Tagbar is a good replacement of taglist
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:tagbar_width = 30
let g:tagbar_autofocus = 1
let g:tagbar_compact = 1
let g:tagbar_singleclick = 1
let g:tagbar_autoshowtag = 1
nnoremap <silent><Leader>tlt :TagbarToggle<CR>


" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" NERDTree settings
" ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent><Leader>ntt :NERDTreeToggle<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" FuzzyFinder settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 400
let g:fuf_mrucmd_maxItem = 400
let g:fuf_mrufile_maxItemDir = 100
let g:fuf_learningLimit = 200
let g:fuf_dataDir = '~/.vim/vim-fuf-data'
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
" highlight Folded guibg=#181818
" I don't like long folddashes, so set fold fillchar to ' '
set fillchars=stl:\ ,stlnc:\ ,vert:\|,fold:\ ,diff:-


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Mark settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
set viminfo+=!  " Save and restore global variables. 
let g:mwDefaultHighlightingPalette = 'maximum'
" we can not use nnoremap with it?
nmap <silent><C-m>m <Plug>MarkToggle
nmap <silent><C-m>c <Plug>MarkAllClear
" we just want to search the current pattern
nmap <Plug>IgnoreMarkSearchNext <Plug>MarkSearchNext
nmap <Plug>IgnoreMarkSearchPrev <Plug>MarkSearchPrev
nmap * <Plug>MarkSearchOrCurNext
nmap # <Plug>MarkSearchOrCurPrev


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" session by Peter Odding settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:session_autosave = 'yes'
let g:session_autoload = 'no'
let g:session_menu = 0

nnoremap <silent><C-s>o :execute "OpenSession "<CR>
nnoremap <silent><C-s>s :execute "SaveSession " . input(":")<CR>
nnoremap <silent><C-s>c :execute "CloseSession "<CR>
nnoremap <silent><C-s>d :execute "DeleteSession "<CR>
nnoremap <silent><C-s>v :execute "ViewSession "<CR>
nnoremap <silent><C-s>r :execute "RestartVim "<CR>


" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" simple_bookmarks settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent><C-b>b :execute "Bookmark " . input(":")<CR>
nnoremap <silent><C-b>g :execute "GotoBookmark " . input(":")<CR>
nnoremap <silent><C-b>d :execute "DelBookmark " . input(":")<CR>
nnoremap <silent><C-b>o :execute "CopenBookmarks"<CR>

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" YankRing settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:yankring_history_dir = '$HOME/.vim'
nnoremap <silent> <C-y>s :YRShow<CR>
nnoremap <silent> <C-y>t :YRToggle<CR>
nnoremap <silent> <C-y>g :YRGetElem<CR>
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

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" DoxygenToolkit settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent> <C-x>dx :Dox<CR>

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" TxtBrowser settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" au BufEnter *.txt setlocal ft=txt

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" JavaScript settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" 打开javascript折叠
let b:javascript_fold=1
" 打开javascript对dom,html和css的支持
let javascript_enable_domhtmlcss=1

autocmd FileType javascript setlocal et sta sw=4 sts=4
autocmd FileType html setlocal et sta sw=4 sts=4

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" smarkword settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge <Plug>(smartword-ge)

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" vim-orgmode settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let maplocalleader = "\\"

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" Screen_vim__gnu_screentmux settings
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let g:ScreenImpl = 'Tmux'
let g:ScreenShellTerminal = 'xfce4-terminal'

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" xml format support
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
autocmd FileType xml setlocal et sta sw=2 sts=2
autocmd FileType docbk setlocal et sta sw=2 sts=2
" XML formatter
function! DoFormatXML() range
	" Save the file type
	let l:origft = &ft

	" Clean the file type
	set ft=

	" Add fake initial tag (so we can process multiple top-level elements)
	exe ":let l:beforeFirstLine=" . a:firstline . "-1"
	if l:beforeFirstLine < 0
		let l:beforeFirstLine=0
	endif
	exe a:lastline . "put ='</PrettyXML>'"
	exe l:beforeFirstLine . "put ='<PrettyXML>'"
	exe ":let l:newLastLine=" . a:lastline . "+2"
	if l:newLastLine > line('$')
		let l:newLastLine=line('$')
	endif

	" Remove XML header
	exe ":" . a:firstline . "," . a:lastline . "s/<\?xml\\_.*\?>\\_s*//e"

	" Recalculate last line of the edited code
	let l:newLastLine=search('</PrettyXML>')

	" Execute external formatter
	exe ":silent " . a:firstline . "," . l:newLastLine . "!xmllint --noblanks --format --recover -"

	" Recalculate first and last lines of the edited code
	let l:newFirstLine=search('<PrettyXML>')
	let l:newLastLine=search('</PrettyXML>')
	
	" Get inner range
	let l:innerFirstLine=l:newFirstLine+1
	let l:innerLastLine=l:newLastLine-1

	" Remove extra unnecessary indentation
	exe ":silent " . l:innerFirstLine . "," . l:innerLastLine "s/^  //e"

	" Remove fake tag
	exe l:newLastLine . "d"
	exe l:newFirstLine . "d"

	" Put the cursor at the first line of the edited code
	exe ":" . l:newFirstLine

	" Restore the file type
	exe "set ft=" . l:origft
endfunction
command! -range=% FormatXML <line1>,<line2>call DoFormatXML()

nmap <silent> <leader>x :%FormatXML<CR>
vmap <silent> <leader>x :FormatXML<CR>

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" The_NERD_Commenter
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let NERDSpaceDelims="1"

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" neocomplete
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"

" For cursor moving in insert mode(Not recommended)
"inoremap <expr><Left>  neocomplete#close_popup() . "\<Left>"
"inoremap <expr><Right> neocomplete#close_popup() . "\<Right>"
"inoremap <expr><Up>    neocomplete#close_popup() . "\<Up>"
"inoremap <expr><Down>  neocomplete#close_popup() . "\<Down>"
" Or set this.
"let g:neocomplete#enable_cursor_hold_i = 1
" Or set this.
"let g:neocomplete#enable_insert_char_pre = 1

" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplete#enable_auto_select = 1
"let g:neocomplete#disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" ModeSelect
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
" vimdiff
" +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nnoremap <silent> <C-x>vd   :execute "vertical diffsplit " . input(": ", "", "file")<CR>
nnoremap <silent> <C-x>sd   :execute "diffsplit " . input(": ", "", "file")<CR>
nnoremap <silent> <C-x>do   :execute "diffoff"<CR>
