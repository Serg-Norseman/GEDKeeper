#if !__MonoCS__

using System;
using System.Collections;
using System.IO;
using System.Runtime.InteropServices;
using System.Security;
using System.Threading;

namespace GKCore.MapiMail
{
    /// <summary>
    /// Represents an email message to be sent through MAPI.
    /// </summary>
    public class MapiMailMessage
    {
        public const int MAPI_USER_ABORT = 1;
        public const int MAPI_E_FAILURE = 2;
        public const int MAPI_E_LOGIN_FAILURE = 3;
        public const int MAPI_E_DISK_FULL = 4;
        public const int MAPI_E_INSUFFICIENT_MEMORY = 5;
        public const int MAPI_E_BLK_TOO_SMALL = 6;
        public const int MAPI_E_TOO_MANY_SESSIONS = 8;
        public const int MAPI_E_TOO_MANY_FILES = 9;
        public const int MAPI_E_TOO_MANY_RECIPIENTS = 10;
        public const int MAPI_E_ATTACHMENT_NOT_FOUND = 11;
        public const int MAPI_E_ATTACHMENT_OPEN_FAILURE = 12;
        public const int MAPI_E_ATTACHMENT_WRITE_FAILURE = 13;
        public const int MAPI_E_UNKNOWN_RECIPIENT = 14;
        public const int MAPI_E_BAD_RECIPTYPE = 15;
        public const int MAPI_E_NO_MESSAGES = 16;
        public const int MAPI_E_INVALID_MESSAGE = 17;
        public const int MAPI_E_TEXT_TOO_LARGE = 18;
        public const int MAPI_E_INVALID_SESSION = 19;
        public const int MAPI_E_TYPE_NOT_SUPPORTED = 20;
        public const int MAPI_E_AMBIGUOUS_RECIPIENT = 21;
        public const int MAPI_E_MESSAGE_IN_USE = 22;
        public const int MAPI_E_NETWORK_FAILURE = 23;
        public const int MAPI_E_INVALID_EDITFIELDS = 24;
        public const int MAPI_E_INVALID_RECIPS = 25;
        public const int MAPI_E_NOT_SUPPORTED = 26;
        public const int MAPI_E_NO_LIBRARY = 999;
        public const int MAPI_E_INVALID_PARAMETER = 998;

        /// <summary>
        /// Specifies the valid RecipientTypes for a Recipient.
        /// </summary>
        public enum RecipientType
        {
            /// <summary>
            /// Recipient will be in the TO list.
            /// </summary>
            To = 1,

            /// <summary>
            /// Recipient will be in the CC list.
            /// </summary>
            CC = 2,

            /// <summary>
            /// Recipient will be in the BCC list.
            /// </summary>
            BCC = 3
        }

        private string _subject;
        private string _body;
        private readonly RecipientCollection _recipientCollection;
        private readonly ArrayList _files;
        private readonly ManualResetEvent _manualResetEvent;

        /// <summary>
        /// Creates a blank mail message.
        /// </summary>
        public MapiMailMessage()
        {
            _files = new ArrayList();
            _recipientCollection = new RecipientCollection();
            _manualResetEvent = new ManualResetEvent(false);
        }

        /// <summary>
        /// Creates a new mail message with the specified subject.
        /// </summary>
        public MapiMailMessage(string subject) : this()
        {
            _subject = subject;
        }

        /// <summary>
        /// Creates a new mail message with the specified subject and body.
        /// </summary>
        public MapiMailMessage(string subject, string body) : this()
        {
            _subject = subject;
            _body = body;
        }

        /// <summary>
        /// Gets or sets the subject of this mail message.
        /// </summary>
        public string Subject
        {
            get { return _subject; }
            set { _subject = value; }
        }

        /// <summary>
        /// Gets or sets the body of this mail message.
        /// </summary>
        public string Body
        {
            get { return _body; }
            set { _body = value; }
        }

        /// <summary>
        /// Gets the recipient list for this mail message.
        /// </summary>
        public RecipientCollection Recipients
        {
            get { return _recipientCollection; }
        }

        /// <summary>
        /// Gets the file list for this mail message.
        /// </summary>
        public ArrayList Files
        {
            get { return _files; }
        }

        /// <summary>
        /// Displays the mail message dialog asynchronously.
        /// </summary>
        public void ShowDialog()
        {
            // Create the mail message in an STA thread
            Thread t = new Thread(new ThreadStart(ShowMail));
            t.IsBackground = true;
            t.SetApartmentState(ApartmentState.STA);
            t.Start();

            // only return when the new thread has built it's interop representation
            _manualResetEvent.WaitOne();
            _manualResetEvent.Reset();
        }

        private const int MAPI_DIALOG = 0x8;
        private const int MAPI_LOGON_UI = 0x1;
        private const int SUCCESS_SUCCESS = 0;

        /// <summary>
        /// Sends the mail message.
        /// </summary>
        private void ShowMail()
        {
            NativeMethods.MapiMessage message = new NativeMethods.MapiMessage();

            using (RecipientCollection.InteropRecipientCollection interopRecipients
                   = _recipientCollection.GetInteropRepresentation())
            {

                message.Subject = _subject;
                message.NoteText = _body;

                message.Recipients = interopRecipients.Handle;
                message.RecipientCount = _recipientCollection.Count;

                // Check if we need to add attachments
                if (_files.Count > 0)
                {
                    // Add attachments
                    message.Files = AllocAttachments(out message.FileCount);
                }

                // Signal the creating thread (make the remaining code async)
                _manualResetEvent.Set();

                int error = (int)NativeMethods.MAPISendMail(IntPtr.Zero, IntPtr.Zero, message, MAPI_DIALOG, 0);

                if (_files.Count > 0)
                {
                    // Deallocate the files
                    DeallocFiles(message);
                }

                // Check for error
                if (error != SUCCESS_SUCCESS)
                {
                    throw new Exception(LogErrorMapi(error));
                }
            }
        }

        /// <summary>
        /// Deallocates the files in a message.
        /// </summary>
        /// <param name="message">The message to deallocate the files from.</param>
        private static void DeallocFiles(NativeMethods.MapiMessage message)
        {
            if (message.Files == IntPtr.Zero) return;

            Type fileDescType = typeof(NativeMethods.MapiFileDescriptor);
            int fsize = Marshal.SizeOf(fileDescType);

            // Get the ptr to the files
            int runptr = (int)message.Files;
            // Release each file
            for (int i = 0; i < message.FileCount; i++)
            {
                Marshal.DestroyStructure((IntPtr)runptr, fileDescType);
                runptr += fsize;
            }
            // Release the file
            Marshal.FreeHGlobal(message.Files);
        }

        /// <summary>
        /// Allocates the file attachments
        /// </summary>
        /// <param name="fileCount"></param>
        /// <returns></returns>
        private IntPtr AllocAttachments(out int fileCount)
        {
            fileCount = 0;
            if (_files == null)
            {
                return IntPtr.Zero;
            }
            if ((_files.Count <= 0) || (_files.Count > 100))
            {
                return IntPtr.Zero;
            }

            Type atype = typeof(NativeMethods.MapiFileDescriptor);
            int asize = Marshal.SizeOf(atype);
            IntPtr ptra = Marshal.AllocHGlobal(_files.Count * asize);

            NativeMethods.MapiFileDescriptor mfd = new NativeMethods.MapiFileDescriptor();
            mfd.position = -1;
            int runptr = (int)ptra;
            for (int i = 0; i < _files.Count; i++)
            {
                string path = _files[i] as string;
                mfd.name = Path.GetFileName(path);
                mfd.path = path;
                Marshal.StructureToPtr(mfd, (IntPtr)runptr, false);
                runptr += asize;
            }

            fileCount = _files.Count;
            return ptra;
        }

        /// <summary>
        /// Logs any Mapi errors.
        /// </summary>
        public string LogErrorMapi(int errorCode)
        {
            string error = string.Empty;
            switch (errorCode)
            {
                case MAPI_USER_ABORT:
                    error = "User Aborted.";
                    break;
                case MAPI_E_FAILURE:
                    error = "MAPI Failure.";
                    break;
                case MAPI_E_LOGIN_FAILURE:
                    error = "Login Failure.";
                    break;
                case MAPI_E_DISK_FULL:
                    error = "MAPI Disk full.";
                    break;
                case MAPI_E_INSUFFICIENT_MEMORY:
                    error = "MAPI Insufficient memory.";
                    break;
                case MAPI_E_BLK_TOO_SMALL:
                    error = "MAPI Block too small.";
                    break;
                case MAPI_E_TOO_MANY_SESSIONS:
                    error = "MAPI Too many sessions.";
                    break;
                case MAPI_E_TOO_MANY_FILES:
                    error = "MAPI too many files.";
                    break;
                case MAPI_E_TOO_MANY_RECIPIENTS:
                    error = "MAPI too many recipients.";
                    break;
                case MAPI_E_ATTACHMENT_NOT_FOUND:
                    error = "MAPI Attachment not found.";
                    break;
                case MAPI_E_ATTACHMENT_OPEN_FAILURE:
                    error = "MAPI Attachment open failure.";
                    break;
                case MAPI_E_ATTACHMENT_WRITE_FAILURE:
                    error = "MAPI Attachment Write Failure.";
                    break;
                case MAPI_E_UNKNOWN_RECIPIENT:
                    error = "MAPI Unknown recipient.";
                    break;
                case MAPI_E_BAD_RECIPTYPE:
                    error = "MAPI Bad recipient type.";
                    break;
                case MAPI_E_NO_MESSAGES:
                    error = "MAPI No messages.";
                    break;
                case MAPI_E_INVALID_MESSAGE:
                    error = "MAPI Invalid message.";
                    break;
                case MAPI_E_TEXT_TOO_LARGE:
                    error = "MAPI Text too large.";
                    break;
                case MAPI_E_INVALID_SESSION:
                    error = "MAPI Invalid session.";
                    break;
                case MAPI_E_TYPE_NOT_SUPPORTED:
                    error = "MAPI Type not supported.";
                    break;
                case MAPI_E_AMBIGUOUS_RECIPIENT:
                    error = "MAPI Ambiguous recipient.";
                    break;
                case MAPI_E_MESSAGE_IN_USE:
                    error = "MAPI Message in use.";
                    break;
                case MAPI_E_NETWORK_FAILURE:
                    error = "MAPI Network failure.";
                    break;
                case MAPI_E_INVALID_EDITFIELDS:
                    error = "MAPI Invalid edit fields.";
                    break;
                case MAPI_E_INVALID_RECIPS:
                    error = "MAPI Invalid Recipients.";
                    break;
                case MAPI_E_NOT_SUPPORTED:
                    error = "MAPI Not supported.";
                    break;
                case MAPI_E_NO_LIBRARY:
                    error = "MAPI No Library.";
                    break;
                case MAPI_E_INVALID_PARAMETER:
                    error = "MAPI Invalid parameter.";
                    break;
            }

            return ("Error sending MAPI Email. Error: " + error + " (code = " + errorCode + ").");
        }

    }

    /// <summary>
    /// Represents a Recipient for a MapiMailMessage.
    /// </summary>
    public class Recipient
    {
        /// <summary>
        /// The email address of this recipient.
        /// </summary>
        public string Address = null;

        /// <summary>
        /// The display name of this recipient.
        /// </summary>
        public string DisplayName = null;

        /// <summary>
        /// How the recipient will receive this message (To, CC, BCC).
        /// </summary>
        public MapiMailMessage.RecipientType RecipientType = MapiMailMessage.RecipientType.To;

        /// <summary>
        /// Creates a new recipient with the specified address.
        /// </summary>
        public Recipient(string address)
        {
            Address = address;
        }

        /// <summary>
        /// Creates a new recipient with the specified address and display name.
        /// </summary>
        public Recipient(string address, string displayName)
        {
            Address = address;
            DisplayName = displayName;
        }

        /// <summary>
        /// Creates a new recipient with the specified address and recipient type.
        /// </summary>
        public Recipient(string address, MapiMailMessage.RecipientType recipientType)
        {
            Address = address;
            RecipientType = recipientType;
        }

        /// <summary>
        /// Creates a new recipient with the specified address, display name and recipient type.
        /// </summary>
        public Recipient(string address, string displayName, MapiMailMessage.RecipientType recipientType)
        {
            Address = address;
            DisplayName = displayName;
            RecipientType = recipientType;
        }

        /// <summary>
        /// Returns an interop representation of a recepient.
        /// </summary>
        /// <returns></returns>
        internal NativeMethods.MapiRecipDesc GetInteropRepresentation()
        {
            NativeMethods.MapiRecipDesc interop = new NativeMethods.MapiRecipDesc();

            if (DisplayName == null)
            {
                interop.Name = Address;
            }
            else
            {
                interop.Name = DisplayName;
                interop.Address = Address;
            }

            interop.RecipientClass = (int)RecipientType;

            return interop;
        }
    }

    /// <summary>
    /// Represents a collection of recipients for a mail message.
    /// </summary>
    public class RecipientCollection : CollectionBase
    {
        /// <summary>
        /// Adds the specified recipient to this collection.
        /// </summary>
        public void Add(Recipient value)
        {
            List.Add(value);
        }

        /// <summary>
        /// Adds a new recipient with the specified address to this collection.
        /// </summary>
        public void Add(string address)
        {
            Add(new Recipient(address));
        }

        /// <summary>
        /// Adds a new recipient with the specified address and display name to this collection.
        /// </summary>
        public void Add(string address, string displayName)
        {
            Add(new Recipient(address, displayName));
        }

        /// <summary>
        /// Adds a new recipient with the specified address and recipient type to this collection.
        /// </summary>
        public void Add(string address, MapiMailMessage.RecipientType recipientType)
        {
            Add(new Recipient(address, recipientType));
        }

        /// <summary>
        /// Adds a new recipient with the specified address, display name and recipient type to this collection.
        /// </summary>
        public void Add(string address, string displayName, MapiMailMessage.RecipientType recipientType)
        {
            Add(new Recipient(address, displayName, recipientType));
        }

        /// <summary>
        /// Returns the recipient stored in this collection at the specified index.
        /// </summary>
        public Recipient this[int index]
        {
            get { return (Recipient)List[index]; }
        }

        internal InteropRecipientCollection GetInteropRepresentation()
        {
            return new InteropRecipientCollection(this);
        }

        /// <summary>
        /// Structure which contains an interop representation of a collection of recipients.
        /// </summary>
        internal struct InteropRecipientCollection : IDisposable
        {
            private IntPtr _handle;
            private int _count;

            /// <summary>
            /// Default constructor for creating InteropRecipientCollection.
            /// </summary>
            /// <param name="outer"></param>
            public InteropRecipientCollection(RecipientCollection outer)
            {
                _count = outer.Count;

                if (_count == 0)
                {
                    _handle = IntPtr.Zero;
                    return;
                }

                // allocate enough memory to hold all recipients
                int size = Marshal.SizeOf(typeof(NativeMethods.MapiRecipDesc));
                _handle = Marshal.AllocHGlobal(_count * size);

                // place all interop recipients into the memory just allocated
                int ptr = (int)_handle;
                foreach (Recipient native in outer)
                {
                    NativeMethods.MapiRecipDesc interop = native.GetInteropRepresentation();

                    // stick it in the memory block
                    Marshal.StructureToPtr(interop, (IntPtr)ptr, false);
                    ptr += size;
                }
            }

            public IntPtr Handle
            {
                get { return _handle; }
            }

            /// <summary>
            /// Disposes of resources.
            /// </summary>
            public void Dispose()
            {
                if (_handle != IntPtr.Zero)
                {
                    Type type = typeof(NativeMethods.MapiRecipDesc);
                    int size = Marshal.SizeOf(type);

                    // destroy all the structures in the memory area
                    int ptr = (int)_handle;
                    for (int i = 0; i < _count; i++)
                    {
                        Marshal.DestroyStructure((IntPtr)ptr, type);
                        ptr += size;
                    }

                    // free the memory
                    Marshal.FreeHGlobal(_handle);

                    _handle = IntPtr.Zero;
                    _count = 0;
                }
            }
        }
    }
    
    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    internal static class NativeMethods
    {
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiFileDescriptor
        {
            public int reserved = 0;
            public int flags = 0;
            public int position = 0;
            public string path = null;
            public string name = null;
            public IntPtr type = IntPtr.Zero;
        }

        public const int MAPI_LOGON_UI = 0x1;

        [DllImport("MAPI32.DLL", CharSet = CharSet.Unicode)]
        public static extern int MAPILogon(IntPtr hwnd, string prf, string pw, int flg, int rsv, ref IntPtr sess);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiMessage
        {
            public int Reserved = 0;
            public string Subject = null;
            public string NoteText = null;
            public string MessageType = null;
            public string DateReceived = null;
            public string ConversationID = null;
            public int Flags = 0;
            public IntPtr Originator = IntPtr.Zero;
            public int RecipientCount = 0;
            public IntPtr Recipients = IntPtr.Zero;
            public int FileCount = 0;
            public IntPtr Files = IntPtr.Zero;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiRecipDesc
        {
            public int Reserved = 0;
            public int RecipientClass = 0;
            public string Name = null;
            public string Address = null;
            public int eIDSize = 0;
            public IntPtr EntryID = IntPtr.Zero;
        }

        [DllImport("MAPI32.DLL", CharSet = CharSet.Unicode)]
        public static extern uint MAPISendMail(IntPtr lhSession, IntPtr ulUIParam, MapiMessage lpMessage, uint flFlags, uint ulReserved);
    }
}

#endif
