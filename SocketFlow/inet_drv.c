// ${ERL_TOP}/erts/emulator/drivers/common/inet_drv.c

/* One numberspace for *_REC_* so if an e.g UDP request is issued
** for a TCP socket, the driver can protest.
*/
#define INET_REQ_OPEN          1
#define INET_REQ_CLOSE         2
#define INET_REQ_CONNECT       3
#define INET_REQ_PEER          4
#define INET_REQ_NAME          5
#define INET_REQ_BIND          6
#define INET_REQ_SETOPTS       7
#define INET_REQ_GETOPTS       8
/* #define INET_REQ_GETIX         9  NOT USED ANY MORE */
/* #define INET_REQ_GETIF         10 REPLACE BY NEW STUFF */
#define INET_REQ_GETSTAT       11
#define INET_REQ_GETHOSTNAME   12
#define INET_REQ_FDOPEN        13
#define INET_REQ_GETFD         14
#define INET_REQ_GETTYPE       15
#define INET_REQ_GETSTATUS     16
#define INET_REQ_GETSERVBYNAME 17
#define INET_REQ_GETSERVBYPORT 18
#define INET_REQ_SETNAME       19
#define INET_REQ_SETPEER       20
#define INET_REQ_GETIFLIST     21
#define INET_REQ_IFGET         22
#define INET_REQ_IFSET         23
#define INET_REQ_SUBSCRIBE     24
#define INET_REQ_GETIFADDRS    25
#define INET_REQ_ACCEPT        26
#define INET_REQ_LISTEN        27
#define INET_REQ_IGNOREFD      28


#define sock_open(af, type, proto) \
    make_noninheritable_handle(socket((af), (type), (proto)))
#define sock_close(s)              closesocket((s))
#define sock_shutdown(s, how)      shutdown((s), (how))

#define sock_accept(s, addr, len) \
    make_noninheritable_handle(accept((s), (addr), (len)))
#define sock_connect(s, addr, len) connect((s), (addr), (len))
#define sock_listen(s, b)          listen((s), (b))
#define sock_bind(s, addr, len)    bind((s), (addr), (len))

#define sock_select(D, Flags, OnOff) winsock_event_select(D, Flags, OnOff)


/* TCP requests from Erlang */
static ErlDrvSSizeT tcp_inet_ctl(ErlDrvData e, unsigned int cmd,
				 char* buf, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;

    switch(cmd) {
    case INET_REQ_OPEN: { /* open socket and return internal index */
	int domain;
	DEBUGF(("tcp_inet_ctl(%ld): OPEN\r\n", (long)desc->inet.port));
	if (len != 2) return ctl_error(EINVAL, rbuf, rsize);
	switch(buf[0]) {
	case INET_AF_INET:
	    domain = AF_INET;
	    break;
#if defined(HAVE_IN6) && defined(AF_INET6)
	case INET_AF_INET6:
	    domain = AF_INET6;
	    break;
#else
	case INET_AF_INET6:
	    return ctl_xerror("eafnosupport", rbuf, rsize);
	    break;
#endif
	default:
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	if (buf[1] != INET_TYPE_STREAM) return ctl_error(EINVAL, rbuf, rsize);
	return inet_ctl_open(INETP(desc), domain, SOCK_STREAM, rbuf, rsize);
	break;
    }

    case INET_REQ_FDOPEN: {  /* pass in an open socket */
	int domain;
	DEBUGF(("tcp_inet_ctl(%ld): FDOPEN\r\n", (long)desc->inet.port));
	if (len != 6) return ctl_error(EINVAL, rbuf, rsize);
	switch(buf[0]) {
	case INET_AF_INET:
	    domain = AF_INET;
	    break;
#if defined(HAVE_IN6) && defined(AF_INET6)
	case INET_AF_INET6:
	    domain = AF_INET6;
	    break;
#else
	case INET_AF_INET6:
	    return ctl_xerror("eafnosupport", rbuf, rsize);
	    break;
#endif
	default:
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	if (buf[1] != INET_TYPE_STREAM) return ctl_error(EINVAL, rbuf, rsize);
	return inet_ctl_fdopen(INETP(desc), domain, SOCK_STREAM,
			       (SOCKET) get_int32(buf+2), rbuf, rsize);
	break;
    }

    case INET_REQ_LISTEN: { /* argument backlog */

	int backlog;
	DEBUGF(("tcp_inet_ctl(%ld): LISTEN\r\n", (long)desc->inet.port)); 
        /// ERROR HANDLING /////
	/* if (desc->inet.state == INET_STATE_CLOSED) */
	/*     return ctl_xerror(EXBADPORT, rbuf, rsize); */
	/* if (!IS_OPEN(INETP(desc))) */
	/*     return ctl_xerror(EXBADPORT, rbuf, rsize); */
	/* if (!IS_BOUND(INETP(desc))) */
	/*     return ctl_xerror(EXBADSEQ, rbuf, rsize); */
	/* if (len != 2) */
	/*     return ctl_error(EINVAL, rbuf, rsize); */
	backlog = get_int16(buf);
	if (IS_SOCKET_ERROR(sock_listen(desc->inet.s, backlog)))
	    return ctl_error(sock_errno(), rbuf, rsize);
	desc->inet.state = INET_STATE_LISTENING;
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }


    case INET_REQ_CONNECT: {   /* do async connect */
	int code;
	char tbuf[2];
	unsigned timeout;

	DEBUGF(("tcp_inet_ctl(%ld): CONNECT\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4), Port(2), Address(N) */

	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (IS_CONNECTED(INETP(desc)))
	    return ctl_error(EISCONN, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (IS_CONNECTING(INETP(desc)))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (len < 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	len -= 4;
	if (inet_set_address(desc->inet.sfamily, &desc->inet.remote,
			     buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	
	code = sock_connect(desc->inet.s, 
			    (struct sockaddr*) &desc->inet.remote, len);
	if (IS_SOCKET_ERROR(code) &&
		((sock_errno() == ERRNO_BLOCK) ||  /* Winsock2 */
		 (sock_errno() == EINPROGRESS))) {	/* Unix & OSE!! */
          sock_select(INETP(desc), FD_CONNECT, 1);
	    desc->inet.state = INET_STATE_CONNECTING;
	    if (timeout != INET_INFINITY)
		driver_set_timer(desc->inet.port, timeout);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	}
	else if (code == 0) { /* ok we are connected */
	    desc->inet.state = INET_STATE_CONNECTED;
	    if (desc->inet.active)
		sock_select(INETP(desc), (FD_READ|FD_CLOSE), 1);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	    async_ok(INETP(desc));
	}
	else {
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_BIND:  {      /* bind socket */
	char tbuf[2];
	inet_address local;
	short port;

	DEBUGF(("inet_ctl(%ld): BIND\r\n", (long)desc->port)); 

	/* if (len < 2) */
	/*     return ctl_error(EINVAL, rbuf, rsize); */
	/* if (desc->state != INET_STATE_OPEN) */
	/*     return ctl_xerror(EXBADPORT, rbuf, rsize); */

	/* if (inet_set_faddress(desc->sfamily, &local, buf, &len) == NULL) */
	/*     return ctl_error(EINVAL, rbuf, rsize); */

	if (IS_SOCKET_ERROR(sock_bind(desc->s,(struct sockaddr*) &local, len)))
	    return ctl_error(sock_errno(), rbuf, rsize);

	desc->state = INET_STATE_BOUND;

	if ((port = inet_address_port(&local)) == 0) {
	    len = sizeof(local);
	    sock_name(desc->s, (struct sockaddr*) &local, (unsigned int*)&len);
	    port = inet_address_port(&local);
	}
	port = sock_ntohs(port);
	put_int16(port, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_ACCEPT: {  /* do async accept */
	char tbuf[2];
	unsigned timeout;
	inet_address remote;
	unsigned int n;
	SOCKET s;

	DEBUGF(("tcp_inet_ctl(%ld): ACCEPT\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4) */

	if ((desc->inet.state != INET_STATE_LISTENING && desc->inet.state != INET_STATE_ACCEPTING &&
	     desc->inet.state != INET_STATE_MULTI_ACCEPTING) || len != 4) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}

	timeout = get_int32(buf);

	if (desc->inet.state == INET_STATE_ACCEPTING) {
	    unsigned long time_left = 0;
	    int oid = 0;
	    ErlDrvTermData ocaller = ERL_DRV_NIL;
	    int oreq = 0;
	    unsigned otimeout = 0;
	    ErlDrvTermData caller = driver_caller(desc->inet.port);
	    MultiTimerData *mtd = NULL,*omtd = NULL;
	    ErlDrvMonitor monitor, omonitor;


	    if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) { 
		return ctl_xerror("noproc", rbuf, rsize);
	    }
	    deq_async_w_tmo(INETP(desc),&oid,&ocaller,&oreq,&otimeout,&omonitor);
	    if (otimeout != INET_INFINITY) {
		driver_read_timer(desc->inet.port, &time_left);
		driver_cancel_timer(desc->inet.port);
		if (time_left <= 0) {
		    time_left = 1;
		}
		omtd = add_multi_timer(&(desc->mtd), desc->inet.port, ocaller, 
				       time_left, &tcp_inet_multi_timeout);
	    }
	    enq_old_multi_op(desc, oid, oreq, ocaller, omtd, &omonitor);
	    if (timeout != INET_INFINITY) {
		mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller, 
				      timeout, &tcp_inet_multi_timeout);
	    }
	    enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
	    desc->inet.state = INET_STATE_MULTI_ACCEPTING;
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
	} else if (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
	    ErlDrvTermData caller = driver_caller(desc->inet.port);
	    MultiTimerData *mtd = NULL;
	    ErlDrvMonitor monitor;

	    if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) { 
		return ctl_xerror("noproc", rbuf, rsize);
	    }
	    if (timeout != INET_INFINITY) {
		mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller, 
				      timeout, &tcp_inet_multi_timeout);
	    }
	    enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
 	} else {
	    n = sizeof(desc->inet.remote);
	    s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &n);
	    if (s == INVALID_SOCKET) {
		if (sock_errno() == ERRNO_BLOCK) {
		    ErlDrvMonitor monitor;
		    if (driver_monitor_process(desc->inet.port, driver_caller(desc->inet.port),
					       &monitor) != 0) { 
			return ctl_xerror("noproc", rbuf, rsize);
		    }
		    enq_async_w_tmo(INETP(desc), tbuf, INET_REQ_ACCEPT, timeout, &monitor);
		    desc->inet.state = INET_STATE_ACCEPTING;
		    sock_select(INETP(desc),FD_ACCEPT,1);
		    if (timeout != INET_INFINITY) {
			driver_set_timer(desc->inet.port, timeout);
		    }
		} else {
		    return ctl_error(sock_errno(), rbuf, rsize);
		}
	    } else {
		ErlDrvTermData caller = driver_caller(desc->inet.port);
		tcp_descriptor* accept_desc;
		int err;
		
		if ((accept_desc = tcp_inet_copy(desc,s,caller,&err)) == NULL) {
		    sock_close(s);
		    return ctl_error(err, rbuf, rsize);
		}
		/* FIXME: may MUST lock access_port 
		 * 1 - Port is accessible via the erlang:ports()
		 * 2 - Port is accessible via callers process_info(links)
		 */
		accept_desc->inet.remote = remote;
		SET_NONBLOCKING(accept_desc->inet.s);
#ifdef __WIN32__
		driver_select(accept_desc->inet.port, accept_desc->inet.event, 
			      ERL_DRV_READ, 1);
#endif
		accept_desc->inet.state = INET_STATE_CONNECTED;
		enq_async(INETP(desc), tbuf, INET_REQ_ACCEPT);
		async_ok_port(INETP(desc), accept_desc->inet.dport);
	    }
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
	}
    }
