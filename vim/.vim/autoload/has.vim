" wrapper to backport the nicer has() syntax for simultanous version and
" patch level checking that was introduced in v7.4.236 and fixed in v7.4.237
"
" * <https://github.com/vim/vim/releases/tag/v7.4.236>
" * <https://github.com/vim/vim/releases/tag/v7.4.237>
"
" 'heavily inspired' by:
"   https://sanctum.geek.nz/cgit/dotfiles.git/tree/vim/autoload/has.vim
if has('patch-7.4.237')
    function! has#(feature) abort
        return has(a:feature)
    endfunction
else
    function! has#(feature) abort
        let matchlist = matchlist(
            \ a:feature,
            \ '^patch-\(\d\+\)\.(\d\+\)\.(\d\+\)$'
            \ )
        if empty(matchlist)
            return has(a:feature)
        endif
        let [major, minor, patch] = matchlist[1:3]
        let l:version = major * 100 + minor

        return v:version != l:version
            \ ? v:version > l:version
            \ : has('patch-' . patch)
    endfunction
endif
