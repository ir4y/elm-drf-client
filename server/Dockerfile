FROM python:3-alpine
ENV LIBRARY_PATH=/lib:/usr/lib
RUN apk update && apk add gcc postgresql-dev musl-dev 
RUN mkdir -p /app/user
RUN mkdir /src
WORKDIR /app/user
ADD requirements.txt /app/user/
RUN pip install --upgrade pip
RUN pip install --src /src -r requirements.txt
ADD . /app/user
#RUN python ./manage.py collectstatic --noinput
#RUN python ./manage.py compilemessages
EXPOSE 8000
