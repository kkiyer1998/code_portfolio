/*******************************************************************
 * Network Non-Cached Threaded Proxy: HTTP 1.0  15-213 CMU- kkiyer *
 * Author: Kaustubh Iyer                                           *
********************************************************************/
#include <stdio.h>
#include <string.h>
#include "csapp.h"
/* Recommended max cache and object sizes */
#define MAX_CACHE_SIZE 1049000
#define MAX_OBJECT_SIZE 102400


/* Uncomment this if you wanna turn on debugger */
//#define DEBUG

#ifdef DEBUG
#define dbg_printf(...) printf(__VA_ARGS__);
#else
#define dbg_printf(...);
#endif
/* Function specs: */
void *handle_client(void *connfdp);
int request_from_server(int clientfd,int *serverfd);
int returned_from_server(int clientfd,int serverfd);
void close_files(int *file1, int* file2);
int parse_request(char* buffer,char *method,char *version,
    char *url,char *port,char *filepath);

/* You won't lose style points for including this long line in your code */
static const char *user_agent_hdr = "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:10.0.3) Gecko/20120305 Firefox/10.0.3\r\n";
//Chrome
//Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36


/* main - We open a listening port and wait for a connection
   request (which we accept) Which spawns a thread to handle 
   the client. We do this infinitely
*/
int main(int argc, char** argv)
{
    int listenfd, *connfdp;
    socklen_t clientlen;
    struct sockaddr_storage clientaddr;
    pthread_t tid;

    // Ignore Sigpipes, rather than terminate the process
    Signal(SIGPIPE,SIG_IGN);
    

    if(argc!=2){
        printf("%s\n", "Usage instructions: ./proxy <portno>");
    }

    // Open a port to listen
    listenfd = Open_listenfd(argv[1]);

    dbg_printf("Started Listening...\n")
    
    while (1) {
        clientlen=sizeof(struct sockaddr_storage);
        connfdp = Malloc(sizeof(int));
        *connfdp = Accept(listenfd,(SA *) &clientaddr, &clientlen);
        Pthread_create(&tid, NULL, handle_client, connfdp);

    }
    
    printf("%s", user_agent_hdr);
    return 0;
}

/* handle_client - Parses the client's request and forwards it
   to the server. Also recieves the server's response and forwards
   it to the client.
*/
void *handle_client(void *connfdp){
    dbg_printf("Opened a client.\n");
    int clientfd = *(int*)connfdp;
    int serverfd = -1; 
    int serverrequest,serverresponse;

    // Cutting off all dependencies on the main listener.
    Free(connfdp);
    Pthread_detach(Pthread_self());
    

    //-1 if failed, 1 if passes
    serverrequest = request_from_server(clientfd,&serverfd);

    if(serverrequest == -1){
        close_files(&clientfd,&serverfd);
        Pthread_exit(NULL);
    }
    serverresponse = returned_from_server(clientfd,serverfd);
    if (serverresponse==-1)
    {
        dbg_printf("Failed. Terminating Connection.\n");
    }
    close_files(&clientfd,&serverfd);
    Pthread_exit(NULL);
    return NULL;
}


/* request_from_server - Sends a request to the server from the client
   after parsing it according to the specifications in proxy.div
   Parses the request and writes to the serverfd
*/
int request_from_server(int clientfd, int *serverfd){
    char *http_version = "HTTP/1.0\r\n";
    char *conn_header = "Connection: close\r\n";
    char *proxy_header = "Proxy-Connection: close\r\n";
    int check=0;
    char buffer[MAXLINE],port[MAXLINE];
    char url[MAXLINE],filepath[MAXLINE];
    char method[MAXLINE],version[MAXLINE];
    rio_t clientfile;
    char header[MAXLINE];
    char f_header[MAXLINE];

    Rio_readinitb(&clientfile,clientfd);

    if(!Rio_readlineb(&clientfile,buffer,MAXLINE)){
        return -1;
    }

    // Parses the request so it can forward.
    if(parse_request(buffer,method,version,url,port,filepath)==-1)
        return -1;

    dbg_printf("Filepath: %s \n",filepath);
    if(!strstr(method,"GET")){
        dbg_printf("We only Accept GET Method. \n");
        return -1;
    }

    //Copying to the header we're gonna forward.
    strcpy(header,"Host: ");
    strcat(header,url);
    strcat(header,"\r\n");

    strcpy(f_header,method);
    strcat(f_header," ");
    strcat(f_header,filepath);
    strcat(f_header," ");
    strcat(f_header,http_version);

    
    strcat(f_header,user_agent_hdr);
    strcat(f_header,conn_header);
    strcat(f_header,proxy_header);

    while(Rio_readlineb(&clientfile,buffer,MAXLINE)){
        if(strcmp(buffer,"\r\n")){
            break;
        }
        else if(strstr(buffer,"User-Agent:"))
            continue;
        else if(strstr(buffer,"Connection:"))
            continue;
        else if(strstr(buffer,"Proxy-Connection:"))
            continue;
        else if(strstr(buffer,"Host:")){
            check=1;
            strcpy(f_header,buffer);
        }
        else{
            strcpy(f_header,buffer);
        }
    }

    // Only adds hostname if the client didn't provide it
    if(check==0){
        strcat(f_header,header);    
    }
    strcat(f_header,"\r\n");

    dbg_printf("Header: %s",f_header);

    if((*serverfd = Open_clientfd(url,port))==-1){
        return -1;
    }
    Rio_writen(*serverfd,f_header,strlen(f_header));
    return 1;
}

/* parse_request- Parses the first string given by the client
   decomposing it into its parts. Uses regex to do so.
*/
int parse_request(char* buffer,char *method,char *version,
    char *url,char *port,char *filepath){
    char addr[MAXLINE];
    char urlandport[MAXLINE];
    char protocol[MAXLINE];

    strcpy(filepath,"/");
    strcpy(port,"80");

    dbg_printf("Request: %s",buffer);

    sscanf(buffer,"%s %s %s",method,addr,version);
    dbg_printf("Addr: %s \n",addr);
    sscanf(addr,"%[^:]://%[^/]%s",protocol,urlandport,filepath);
    if(strstr(urlandport,":"))
        sscanf(urlandport,"%[^:]:%s",url,port);
    else
        strcpy(url,urlandport);

    dbg_printf("Parts: Method: %s version: %s urlbeforesplit: %s url: %s path: %s port: %s \n",method,version,urlandport,url,filepath,port);
    if(strlen(url)==0 || strlen(method)==0 || strlen(version)==0){
        dbg_printf("Invalid Request given. Terminating.\n");
        return -1;
    }
    return 0;
}



/* returned_from_server- Reads the server's response
   and forwards it unchanged to the client.
*/
int returned_from_server(int clientfd, int serverfd){
    if(serverfd<0){
        dbg_printf("Server Connection Inactive.\n");
        return -1;
    }
    rio_t serverfile;
    char buffer[MAXLINE];
    int contentsize=0;

    Rio_readinitb(&serverfile,serverfd);
    
    Rio_readlineb(&serverfile,buffer,MAXLINE);
    dbg_printf("Server Response:\n");
    Rio_writen(clientfd,buffer,strlen(buffer));
    while((contentsize=Rio_readlineb(&serverfile,buffer,MAXLINE))!=0){
        Rio_writen(clientfd,buffer,contentsize);
    }
    dbg_printf("End of server response.\n");
    return 0;
}

/* close_files- Closes active file descriptors
*/
void close_files(int *file1,int *file2){
    if(*file1>0){
        Close(*file1);
    }
    if(*file2>0){
        Close(*file2);
    }
}