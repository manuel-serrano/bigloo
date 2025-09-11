/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/bigloo-node.mjs        */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Thu Sep 11 08:44:13 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding, node specific                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync, readFileSync, fstatSync, lstatSync, mkdirSync, readdirSync, ftruncateSync, truncateSync, renameSync, symlinkSync, chmodSync } from "node:fs";
import { isatty } from "node:tty";
import { dirname, extname, sep as file_sep } from "node:path";
import { format } from "node:util";
import { execSync, spawnSync, spawn } from "node:child_process";
import { createServer, createConnection, Socket } from "node:net";

import { BglRuntime, bglParseArgs } from "./bigloo-common.mjs";

/*---------------------------------------------------------------------*/
/*    Text decoders                                                    */
/*---------------------------------------------------------------------*/
const schemeStringDecoder = new TextDecoder();
const schemeStringEncoder = new TextEncoder();

const ucs2StringDecoder = new TextDecoder("ucs-2");
const ucs2StringEncoder = new TextEncoder("ucs-2");


function loadUCS2String(buffer) {
   return ucs2StringDecoder.decode(buffer);
}

/*---------------------------------------------------------------------*/
/*    BglNodeRuntime                                                   */
/*---------------------------------------------------------------------*/
class BglNodeRuntime extends BglRuntime {

   getCurrentLocale() {
      return Intl.DateTimeFormat().resolvedOptions().locale;
   }
   
   stringEncode(str) {
      return schemeStringEncoder.encode(str)
   }

   stringDecode(buffer) {
      return schemeStringDecoder.decode(buffer)
   }

   js_io(self) {
      const charBuffer = new Uint8Array(1);

      return {
	 file_separator: file_sep.charCodeAt(0),
	 
	 open_file: (path_addr, path_length, flags) => {
	    const path = self.loadString(path_addr, path_length);

	    let fs_flags;
	    switch (flags) {
               case 0: // read-only
		  fs_flags = 'r';
		  break;
               case 1: // write-only
		  fs_flags = 'w';
		  break;
               case 2: // write-only in append mode
		  fs_flags = 'r+';
		  break;
               default:
		  throw WebAssembly.RuntimeError("invalid open flags");
	    }
	    try {
               return openSync(path, fs_flags);
	    } catch(e) {
	       return -1;
	    }
	 },

	 open_fd: (fd, flags, addr) => {
	    const buf = fd.toString();
	    self.storeString(buf, addr);
	    return buf.length;
	 },
	 
	 close_file: (fd) => closeSync(fd),

	 close_socket: (sock) => sock.end(),
	 
	 read_file: (fd, offset, length, position) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }
	    const buffer = new Uint8Array(self.memory.buffer, offset, length);
	    return readSync(fd, buffer, 0, length, position < 0 ? -1 : position);
	 },

	 read_socket: (socket, addr, size) => {
	    const buf = socket.read(size, () => console.error("in read..."));
	    if (buf) {
	       return self.storeString(buf, addr);
	    } else {
	       return 0;
	    }
	 },
	 
	 ftruncate: (fd, pos) => {
	    ftruncateSync(fd, pos);
	    return 0;
	 }, 
	 
	 truncate: (path_addr, path_length, pos) => {
	    const path = self.loadString(path_addr, path_length);
	    truncateSync(path, pos);
	    return 0;
	 }, 

	 rename: (old_addr, old_length, new_addr, new_length) => {
	    const oldf = self.loadString(old_addr, old_length);
	    const newf = self.loadSchemeSring(new_addr, new_length);
	    renameSync(oldf, newf);
	    return 0;
	 },
	 
	 symlink: (target_addr, target_length, path_addr, path_length) => {
	    const target = self.loadString(target_addr, target_length);
	    const path = self.loadString(path_addr, path_length);
	    symlinkSync(target, path);
	    return 0;
	 },
	 
	 password: (prompt_addr, prompt_length, res_addr) => {
	    const prompt = self.loadString(prompt_addr, prompt_length);
	    const buf = "toto";

	    self.storeString(buf, addr);
	    return buf.length;
	 },
	 
	 path_size: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).size;
	    } catch (err) {
               return -1;
	    }
	 },

	 last_modification_time: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).mtime;
	    } catch (err) {
               return -1;
	    }
	 },

	 last_access_time: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).atime;
	    } catch (err) {
               return -1;
	    }
	 },

	 last_change_time: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).ctime;
	    } catch (err) {
               return -1;
	    }
	 },

	 utime: (path_addr, path_length, atime, mtime) => {
	    const path = self.loadString(path_addr, path_length);

	    return utime(path, atime, mtime);
	 },

	 file_size: (fd) => {
	    try {
	       return fstatSync(fd).size;
	    } catch (err) {
               return -1;
	    }
	 },

	 path_mode: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).mode;
	    } catch (err) {
               return -1;
	    }
	 },

	 bgl_chmod: (path_addr, path_length, read, write, exec) => {
	    const path = self.loadString(path_addr, path_length);
	    const mod = (read ? constants.S_IRUSR : 0)
	       | (write ? constants.S_IWUSR : 0)
	       | (exec ? constants.S_IXUSR : 0);

	    return chmodSync(path, mod);
	 },

	 chmod: (path_addr, path_length, mod) => {
	    const path = self.loadString(path_addr, path_length);

	    return chmodSync(path, mod);
	 },

	 path_gid: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).gid;
	    } catch (err) {
               return -1;
	    }
	 },

	 path_uid: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return lstatSync(path).uid;
	    } catch (err) {
               return -1;
	    }
	 },

	 path_type: (path_addr, path_length, addr) => {
	    const path = self.loadString(path_addr, path_length);
	    let res;
	    
	    try {
	       const s = lstatSync(path);

	       if (s.isBlockDevice()) {
		  res = "block";
	       } else if (s.isCharacterDevice()) {
		  res = "character";
	       } else if (s.isDirectory()) {
		  res = "directory";
	       } else if (s.FIFO()) {
		  res = "fifo";
	       } else if (s.isFile()) {
		  res = "regular";
	       } else if (s.isSocket()) {
		  res = "socket";
	       } else if (s.isSymbolLink()) {
		  res = "link";
	       }
	    } catch (err) {
               res = "does-not-exist";
	    }
	    self.storeString(res, addr)
	    return res.length;
	 },

	 isatty: (fd) => {
	    return isatty(fd);
	 },
	 
	 file_exists: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
               accessSync(path, constants.F_OK);
               return 1;
	    } catch (err) {
               return 0;
	    }
	 },

	 file_delete: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       if (existsSync(path)) {
		  unlinkSync(path);
		  return false;
	       } else {
		  return true;
	       }
	    } catch (err) {
               return true;
	    }
	 },

	 dir_remove: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
               rmdirSync(path);
               return false;
	    } catch (err) {
               return true;
	    }
	 },

	 is_dir: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);

	    try {
               return lstatSync(path).isDirectory();
	    } catch (err) {
               return false;
	    }
	 },

	 make_dir: (path_addr, path_length, mod) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       mkdirSync(path, { mod: mod });
	       return true;
	    } catch(e) {
	       return false;
	    }
	 },

	 append_file: (fd, offset, length) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }

	    const buffer = new Uint8Array(self.memory.buffer, offset, length);
	    return writeSync(fd, buffer, 0, length);
	 },
	 
	 write_file: (fd, offset, length, position) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }

	    const buffer = new Uint8Array(self.memory.buffer, offset, length);
	    return writeSync(fd, buffer, 0, length, position);
	 },
	 
	 write_socket: (socket, offset, length) => {
	    const buffer = new Uint8Array(self.memory.buffer, offset, length);
	    return socket.write(buffer, () => console.error("*** written..."));
	 },
	 
	 append_char: (fd, c) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }
	    charBuffer[0] = c;
	    return writeSync(fd, charBuffer, 0, 1);
	 },

	 write_char: (fd, c, position) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }
	    charBuffer[0] = c;
	    return writeSync(fd, charBuffer, 0, 1, position);
	 },

	 write_bignum: (fd, n) => {
	    if (fd < 0) {
               throw WebAssembly.RuntimeError("invalid file descriptor");
	    }
	    return writeSync(fd, n.toString());
	 },

	 read_dir_init: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return readdirSync(path);
	    } catch(e) {
	       return null;
	    }
	 },
	 read_dir_size: (dir) => dir.length,
	 read_dir_entry: (dir, num, addr) => {
	    return self.storeString(dir[num], addr);
	 },

	 mmap_init: (path_addr, path_length, read, write) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return readFile(path);
	    } catch(e) {
	       return null;
	    }
	 },
      };
   }

   js_socket(self) {
      return {
	 nullsocket: () => {
	    return undefined;
	 },

	 make_server: (host_addr, host_len, portnum, backlog, family) => {
	    const server = createServer((socket) => {
	       socket.on('data', (data) => {
		  console.log('Received:', data.toString());
	       });
	       
	       socket.on('end', () => {
		  console.log('Client disconnected');
	       });
	    });
	    server.listen(portnum, () => { console.log("server listening"); });
	    return server;
	 },

	 accept: async (srv) => {
	    console.error("XXX in accept");
	    const r = await new Promise((res, rej) => {
	       console.error("in promise...");
	       res(343);
	    });
	    return r;
	 },

	 make_client: (host_addr, host_len, port, timeout) => {
	    const host = self.loadString(host_addr, host_len);
	    console.log("host=[" + host + "] port=" + port);
	    return createConnection({ host, port }, () => { console.log("connected"); });
	 }
      };
   }

   js_process(self) {
      return {
	 nullprocess: () => {
	    return undefined;
	 },

	 run: (nbargs, addr, fork, wait, in_addr, in_len, out_addr, out_len, err_addr, err_len) => {
	    let res;
	    const args = new Array(nbargs);
	    let { addr: naddr, str: cmd } = self.loadStringLen2(addr);
	    let stdin = -1, stdout = -1, stderr = -1;
	    const opt = {
	       stdio: ['inherit', 'inherit', 'inherit']
	    };

	    // arguments
	    for (let i = 0; i < nbargs; i++) {
	       const { addr: a, str } = self.loadStringLen2(naddr);
	       args[i] = str;
	       naddr = a;
	    }

	    // stdin
	    if (in_addr > 0) {
	       // file
	       const path = self.loadString(in_addr, in_len);
	       stdin = openSync(path, "r");
	       opt.stdio[0] = stdin;
	    } else if (in_addr === -1) {
	       // pipe
	       opt.stdio[0] = 'pipe';
	    }

	    
	    // stdio
	    if (out_addr > 0) {
	       // file
	       const path = self.loadString(out_addr, out_len);
	       stdout = openSync(path, "w");
	       opt.stdio[1] = stdout;
	    } else if (out_addr === -1) {
	       // pipe
	       opt.stdio[1] = 'pipe';
	    }

	    // stderr
	    if (err_addr > 0) {
	       // file
	       const path = self.loadString(err_addr, err_len);
	       stderr = openSync(path, "w");
	       opt.stdio[2] = stderr;
	    } else if (err_addr === -1) {
	       // pipe
	       opt.stdio[2] = 'pipe';
	    }

	    if (wait === 1) {
	       try {
		  res = spawnSync(cmd, args, opt);
		  res.alive = 0;
	       } catch(e) {
		  res.status = e.status;
		  opt.stdio.forEach(v => { if (typeof v === "number") closeSync(v); });
	       }

	       if (stdout > 0) {
		  closeSync(stdout);
	       }
	       if (!fork) {
		  process.exit(0);
	       }
	    } else {
	       res = { proc: spawn(cmd, args, opt), alive: 1 };
	       res.proc.on('close', code => {
		  console.log("C=", code);
		  res.alive = 0;
		  res.status = code;
	       });
	    }

	    return res;
	 },

	 xstatus: proc => proc.status ? proc.status : -1,

	 alive: proc => proc.alive,
	 
	 getinport: (proc, addr) => {
	    if (proc.output && proc.output[0]) {
	       const s = proc.output[0].toString();
	       self.storeString(s, addr);
	       return s.length;
	    } else if (proc.proc && proc.proc.stdin instanceof Socket) {
	       return -1;
	    } else {
	       return -2;
	    }
	 },
	 
	 getoutport: (proc, fd, addr) => {
	    if (proc.output && proc.output[fd] ) {
	       const s = proc.output[fd].toString();
	       self.storeString(s, addr);
	       return s.length;
	    } else if (proc.proc && proc.proc.stdio[fd] instanceof Socket) {
	       return -1;
	    } else {
	       return -2;
	    }
	 },
	 
	 getportsock: (proc, fd) => proc.proc[["stdin", "stdout", "stderr"][fd]],
	 
	 pid: proc => proc.proc ? proc.proc.pid : -1,

	 kill: proc => proc.proc ? proc.proc.kill() : 0
      };
   }

   js_system(self) {
      return {
	 argc: argv.length - 2,
	 
	 command_line_size: () => process.argv.length,
	 
	 command_line_entry: (num, addr) => self.storeString(process.argv[num], addr),
	 
	 executable_name: (addr) => self.storeString(process.argv[0], addr),
	 
	 get_arg: (idx, addr) => {
	    let real_idx = idx + 2;
	    let arg = argv[real_idx];
	    return self.storeString(arg, addr);
	 },
	 
	 getenv: (addr, len) => {
	    const v = self.loadString(addr, len);

	    if (v in process.env) {
	       self.storeString(process.env[v], addr);
	       return process.env[v].length;
	    } else {
	       return -1;
	    } 
	 },

	 getenv_len: () => {
	    return Object.keys(process.env).length;
	 },
	 
	 getenv_var: (i, addr) => {
	    const val = process.env[Object.keys(process.env)];
	    self.storeScheme(val, addr);
	    return val.length;
	 },
	 
	 setenv: (addr_id, len_id, addr_val, len_val, addr) => {
	    const id = self.loadtring(id_addr, id_length);
	    const val = self.loadString(val_addr, val_length);
	    process.env[id] = val;
	    return 0;
	 },

	 getcwd: (addr) => {
	    const s = process.cwd();
	    self.storeString(s, addr);
	    return s.length;
	 },

	 date: (addr) => {
	    const d = Date();
	    const a = Days[d.getDay()];
	    const m = Months[d.getMonth()];
	    const b = `${a} ${m} ${d.getDay()} ${d.getHours()}:${d.getMinutes()}:${d.getSeconds()} ${d.getYear()}`
	    self.storeString(b, addr);
	    return b.length;
	 },

	 umask: (mask) => {
	    return process.umask(mask);
	 },
	 
	 chdir: (addr, len) => {
	    const v = self.loadString(addr, len);
	    return Process.chdir(v);
	 },
	 
	 system: (addr, len) => {
	    const v = self.loadString(addr, len);

	    try {
	       execSync(v);
	       return 0;
	    } catch (e) {
	       return 1;
	    }
	 },
	 
	 exit: process.exit,

	 sleep: async (tmt) => new Promise((res, rej) => setTimeout(res, tmt)),
	 
	 signal: (sig, hdl) => {
	    // console.log("NOT IMPLEMENTED SIGNAL sig=", sig, "hdl=", hdl);
	 }
      };
      return self;
   }
}

/*---------------------------------------------------------------------*/
/*    runStatic ...                                                    */
/*    -------------------------------------------------------------    */
/*    Run a whole wasm program in a single self.instance.              */
/*---------------------------------------------------------------------*/
async function runStatic(client) {
   const __js = new BglNodeRuntime();
   const wasmClient = await WebAssembly.compile(readFileSync(client));
   const instanceClient = await WebAssembly.instantiate(wasmClient, __js);
   
   __js.link(instanceClient, instanceClient);

   if (!instanceClient.exports.bigloo_main) {
      console.error(`*** ERROR: missing 'bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   if (!instanceClient.exports.__bigloo_main) {
      console.error(`*** ERROR: missing '__bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   instanceClient.exports.__bigloo_main();
}

/*---------------------------------------------------------------------*/
/*    libRuntime ...                                                   */
/*---------------------------------------------------------------------*/
async function libRuntime(lib) {
   lib.rts = new BglNodeRuntime();
   
   if (lib.js !== "none") {
      const path = `${dirname(lib.lib)}/${lib.js}-node.mjs`;
      try {
	 const mod = await import(path);
	 mod.init(lib.rts);
      } catch(e) {
	 console.error(`*** ERROR:${process.argv[0]}:${path}`);
	 console.error(e);
	 process.exit(1);
      }
   }

   return lib.rts;
}

/*---------------------------------------------------------------------*/
/*    runDynamic ...                                                   */
/*    -------------------------------------------------------------    */
/*    Run a wasm in several instances, one for client, and one by libs */
/*---------------------------------------------------------------------*/
async function runDynamic(client, rts, libs) {
   const __jsRts = new BglNodeRuntime();
   const __jsLibs = await Promise.all(libs.map(libRuntime));
   const __jsClient = new BglNodeRuntime();

   const wasmRts = await WebAssembly.compile(readFileSync(rts));
   const wasmLibs = await Promise.all(libs.map(l => WebAssembly.compile(readFileSync(l.lib))));
   const wasmClient = await WebAssembly.compile(readFileSync(client));

   const instanceRts = await WebAssembly.instantiate(wasmRts, __jsRts);
   __jsLibs.forEach(l => l.__bigloo = instanceRts.exports);
   __jsClient.__bigloo = instanceRts.exports;

   const instanceLibs = await Promise.all(libs.map((l, i) => WebAssembly.instantiate(wasmLibs[i], __jsLibs[i])));
   libs.forEach((l, i) => __jsClient[l.exports] = instanceLibs[i].exports);
   const instanceClient = await WebAssembly.instantiate(wasmClient, __jsClient);

   __jsClient.link(instanceClient);
   libs.forEach((l, i) => __jsLibs[i].link(instanceRts));
   __jsRts.link(instanceRts, instanceClient);
   
   if (!instanceClient.exports.bigloo_main) {
      console.error(`*** ERROR: missing 'bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   if (!instanceRts.exports.__bigloo_main) {
      console.error(`*** ERROR: missing '__bigloo_main' export in "${rts}".`);
      process.exit(1);
   }

   instanceRts.exports.__bigloo_main();
}

/*---------------------------------------------------------------------*/
/*    Minimalist command line parsing                                  */
/*---------------------------------------------------------------------*/
const argv = (globalThis.window && "Deno" in window)
   ? (await import('node:process')).argv
   : process.argv;

const { client, rts, libs } = bglParseArgs(argv);


/*---------------------------------------------------------------------*/
/*    top-level                                                        */
/*---------------------------------------------------------------------*/
try {
   if (rts) {
      await runDynamic(client, rts, libs);
   } else {
      runStatic(client);
   }
} catch(e) {
   console.error("*** ERROR", e);
}
