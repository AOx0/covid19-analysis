set dotenv-load := true
alias r := reset
alias o := open

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