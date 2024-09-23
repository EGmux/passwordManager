FROM  clfoundation/sbcl:latest as builder

COPY . /root/common-lisp/
WORKDIR /root/common-lisp/

RUN cp /usr/local/share/common-lisp/source/quicklisp/quicklisp.lisp /root/common-lisp/

ENV QUICKLISP_ADD_TO_INIT_FILE=true
RUN sbcl --non-interactive --load /root/common-lisp/quicklisp.lisp \
    --eval "(quicklisp-quickstart:install)" \
    --eval "(ql-util:without-prompting (ql:add-to-init-file))"
    
RUN sbcl --non-interactive --eval "(ql:quickload :passwordmanagerapp)" \
       # --load passwordmanager.lisp \
       # --load passwordmanagerguiclog.lisp \ 
       --eval "(sb-ext:save-lisp-and-die \"core\" :toplevel #'passwordmanagergui::main :executable t)"

# FROM clfoundation/sbcl:latest


RUN adduser --disabled-password app
     
# COPY --from=builder /root/common-lisp/ .

# RUN chmod 777 -R /root/common-lisp/quicklisp/dists/quicklisp/software/clog-20231021-git/static-files

EXPOSE 4040

ENTRYPOINT ["sbcl",  "--core", "core"]

