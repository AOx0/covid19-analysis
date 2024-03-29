set dotenv-load := true
alias r := reset
alias o := open
alias c := commit
alias ca := commit_all
alias p := push
alias cp := commit_push

default: publish commit push
    
publish:
    cd $PAGE git rm -rf covid #! Remove everything to ger rid of unused media
    cp -rf covid $PAGE
    cd $PAGE && git add covid

    cd $PAGE && git commit -am "Updated covid"
    cd $PAGE && git push 

@reset:
    rm -rf out
    mkdir out

@open:
    open covid/index.html -a Safari

@commit:
    git rm -r --cached "Writeup.ulysses" #! Remove everything to ger rid of unused media
    git add "Writeup.ulysses"
    git commit -am "Ulysses doc update"

@commit_all msg:
    git commit -am "{{msg}}"

@push:
    git push

@commit_push msg: (commit_all msg) push
    