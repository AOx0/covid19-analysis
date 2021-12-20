set dotenv-load := true
alias r := reset
alias o := open
alias c := commit

default: publish

push:
    cd $PAGE && git commit -am "Updated covid"
    cd $PAGE && git push 

publish: && push
    rm -rf $PAGE/covid
    cp -rf out $PAGE/covid

@reset:
    rm -rf out
    mkdir out

@open:
    open out/index.html -a Safari

@commit:
    git rm -r --cached "Writeup.ulysses"
    git add "Writeup.ulysses"
    git commit -am "Ulysses doc update"