/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/bigloo-mozjs.mjs       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Sep  9 08:38:49 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding, node specific                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { BglRuntime, bglParseArgs } from "./bigloo-common.mjs";

/*---------------------------------------------------------------------*/
/*    Compatibility kit                                                */
/*---------------------------------------------------------------------*/
const process = {
   argv: ["js", "runtime-mozjs.mjs"].concat(scriptArgs),
   env: { HOME: os.getenv("HOME") },
   exit: n => quit(n > 127 ? 1 : n),
   cwd: () => os.getenv("PWD")
}

console.error = console.log;

function readFileSync(o) {
   return os.file.readFile(o, "binary");
}

function writeSync(fd, buffer, offset, length) {
   const buf = buffer.subarray(offset, length);
   const str = Array.from(buf, byte => String.fromCharCode(byte)).join('')
   
   if (fd === 1) {
      putstr(str);
   } else {
      os.file.writeTypedArrayToFile("/tmp/mozjs.out", buf);
      os.system(`cat /tmp/mozjs.out 1>&${fd}`);
   }
}

function existsSync(path) {
   if (os?.file?.exists) {
      return os.file.exists(path);
   } else {
      try {
	 read(path);
	 return true;
      } catch (e) {
	 return false;
      }
   }
}

function extname(path) {
   if (typeof path !== 'string' || path === '') return '';

   const lastSlash = path.lastIndexOf('/');
   const lastDot = path.lastIndexOf('.');

   if (lastDot <= lastSlash) return '';

   if (lastDot === path.length - 1) return '.';

   return path.slice(lastDot);
}

/*---------------------------------------------------------------------*/
/*    BglMozRuntime                                                    */
/*---------------------------------------------------------------------*/
class BglMozRuntime extends BglRuntime {

   getCurrentLocale() {
      return os.getenv("LC_ALL")
	 || os.getenv("LC_MESSAGES")
	 || os.getenv("LANG")
	 || os.getenv("LANGUAGE")
	 || "en_US";
   }
   
   stringEncode(str) {
      const buf = new Uint8Array(str.length);
      for (let i = 0; i < str.length; i++) {
	 buf[i] = str.charCodeAt(i);
      }
      return buf;
   }

   stringDecode(buffer) {
      return Array.from(buffer, byte => String.fromCharCode(byte)).join('');
   }

   js_io(self) {
      return {
	 file_separator: 47,
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
	    const memory = new Uint8Array(self.memory.buffer, offset, length, position);
	    const nbread = readSync(fd, memory, 0, length, position);

	    return nbread;
	 },

	 read_socket: (socket, addr, size) => {
	    return 0;
	 },
	 
	 write_socket: (socket, offset, length) => {
	    return 0;
	 },
	 
	 password: (prompt_addr, prompt_length, res_addr) => {
	    const memory = new Uint8Array(self.memory.buffer, offset, length, position);
	    const buf = "toto";

	    self.storeString(buf, addr);
	    return buf.length;
	 },

	 ftruncate: (fd, pos) => {
	    return 0;
	 }, 
	 
	 truncate: (path_addr, path_length, pos) => {
	    const path = self.loadString(path_addr, path_length);
	    return 0;
	 }, 
	 
	 rename: (old_addr, old_length, new_addr, new_length) => {
	    const oldf = self.loadString(old_addr, old_length);
	    const newf = self.loadString(new_addr, new_length);
	    return 0;
	 },
	 
	 symlink: (target_addr, target_length, path_addr, path_length) => {
	    const target = self.loadString(target_addr, target_length);
	    const path = self.loadString(path_addr, path_length);
	    return 0;
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

	    return 0;
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
	       return fstatSync(fd).mode;
	    } catch (err) {
               return -1;
	    }
	 },

	 bgl_chmod: (path_addr, path_length, read, write, exec) => {
	    const path = self.loadString(path_addr, path_length);

	    return 0;
	 },

	 chmod: (path_addr, path_length, mod) => {
	    const path = self.loadString(path_addr, path_length);

	    return 0;
	 },

	 path_gid: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return fstatSync(fd).gid;
	    } catch (err) {
               return -1;
	    }
	 },

	 path_uid: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
	       return fstatSync(fd).uid;
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
	    return false;
	 },
	 
	 file_exists: (path_addr, path_length) => {
	    const path = self.loadString(path_addr, path_length);
	    try {
               accessSync(path, constants.F_OK);
               return true;
	    } catch (err) {
               return false;
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
	       mkdirSync(path, {mod: mod});
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
	 }
      }
   }

   js_socket(self) {
      return {
	 self: undefined,

	 nullsocket: () => {
	    return undefined;
	 },
	 
	 make_server: (hostname_addr, hostname_len, portnum, backlog, family) => {
	    return undefined;
	 },

	 accept: (srv) => {
	    return 0;
	 },

	 make_client: (hostname_addr, hostname_len, portnum, timeout) => {
	    return undefined;
	 }
      };
   }

   js_process(self) {
      return {
	 nullprocess: () => {
	    return undefined;
	 },
	 
	 run: (nbargs, addr, fork, wait, out_addr, out_len) => {
	 },
	 
	 xstatus: proc => -1,

	 alive: proc => 0,
	 
	 getoutport: (proc, fd, addr) => 1,
	 
	 getinport: (proc, addr) => 0,

	 getportsock: (proc, fd) => undefined,
	 
	 pid: proc => proc.pid,

	 kill: proc => proc.kill()
      };
   }

   js_system(self) {
      return {
	 argc: argv.length - 2,
	 
	 command_line_size: () => process.argv.length,
	 
	 command_line_entry: (num, addr) => self.storeString(process.argv[num], addr),
	 
	 executable_name: (addr) => self.storeString(process.argv[0], addr),
	 
	 get_arg: function (idx, addr) {
            let arg = argv[idx + 2];
            return self.storeString(arg, addr);
	 },

	 getcwd: (addr) => {
	    const s = process.cwd();
	    self.storeString(s, addr);
	    return s.length;
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
	    self.storeString(val, addr);
	    return val.length;
	 },
	 
	 setenv: (addr_id, len_id, addr_val, len_val, addr) => {
	    const id = self.loadString(id_addr, id_length);
	    const val = self.loadString(val_addr, val_length);
	    process.env[id] = val;
	    return 0;
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
	    return 0;
	 },
	 
	 chdir: (mask) => {
	    const v = self.loadString(addr, len);

	    return 0;
	 },
	 
	 system: (addr, len) => {
	    const v = self.loadString(addr, len);
	    return 1;
	 },
	 
	 exit: process.exit,

	 sleep: async (tmt) => new Promise((res, rej) => setTimeout(res, tmt)),
	 
	 signal: (sig, hdl) => {
	    // console.log("NOT IMPLEMENTED SIGNAL sig=", sig, "hdl=", hdl);
	 }
      };
   }
}

/*---------------------------------------------------------------------*/
/*    runStatic ...                                                    */
/*    -------------------------------------------------------------    */
/*    Run a whole wasm program in a single self.instance.              */
/*---------------------------------------------------------------------*/
async function runStatic(client) {
   const __js = new BglMozRuntime();
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

   instanceClient.exports.__bigloo_main(1);
}

/*---------------------------------------------------------------------*/
/*    runDynamic ...                                                   */
/*    -------------------------------------------------------------    */
/*    Run a wasm in two instances, one for client, one the runtime.    */
/*---------------------------------------------------------------------*/
async function runDynamic(client, rts, libs) {
   const __jsClient = new BglMozRuntime();
   const __jsLibs = libs.map(l => new BglMozRuntime());
   const __jsRts = new BglMozRuntime();
   
   const wasmRts = new WebAssembly.Module(readFileSync(rts));
   const wasmLibs = libs.map(l => new WebAssembly.Module(readFileSync(l.lib)));
   const wasmClient = new WebAssembly.Module(readFileSync(client));

   const instanceRts = new WebAssembly.Instance(wasmRts, __jsRts);
   __jsLibs.forEach(l => l.__bigloo = instanceRts.exports);
   __jsClient.__bigloo = instanceRts.exports;

   const instanceLibs = __jsLibs.map((l, i) => new WebAssembly.Instance(wasmLibs[i], __jsLibs[i]));
   libs.forEach((l, i) => __jsClient[l.exports] = instanceLibs[i].exports);
   const instanceClient = new WebAssembly.Instance(wasmClient, __jsClient);

   __jsClient.link(instanceClient, instanceClient);
   libs.forEach((l, i) => __jsLibs[i].link(instanceClient, instanceClient));
   __jsRts.link(instanceRts, instanceClient);
   
   if (!instanceClient.exports.bigloo_main) {
      console.error(`*** ERROR: missing 'bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   if (!instanceRts.exports.__bigloo_main) {
      console.error(`*** ERROR: missing '__bigloo_main' export in "${rts}".`);
      process.exit(1);
   }

   try {
      instanceRts.exports.__bigloo_main(1);
   } catch(e) {
      putstr("*** WASM ");
      print(e.toString());
      process.exit(1);
   }
}

/*---------------------------------------------------------------------*/
/*    Minimalist command line parsing                                  */
/*---------------------------------------------------------------------*/
const argv = process.argv;

const { client, rts, libs } = bglParseArgs(argv);

/*---------------------------------------------------------------------*/
/*    top-level                                                        */
/*---------------------------------------------------------------------*/
try {
   if (rts) {
      await runDynamic(client, rts, libs);
   } else {
      await runStatic(client);
   }
      
} catch(e) {
   print("*** ERROR", e);
   print(e.stack);
   quit(4);
}

// js128 -P wasm_gc -P wasm_exnref -P wasm_tail_calls runtime-mozjs.mjs a.out.wasm
