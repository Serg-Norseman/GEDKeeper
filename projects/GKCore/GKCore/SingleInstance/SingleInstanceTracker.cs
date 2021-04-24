/**
 * Original SingleInstancingWithIpc code by Shy Agam
 * (http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution)
 */

#if !__MonoCS__
#define IPC_SUPPORTS
#endif

#if NETSTANDARD
#undef IPC_SUPPORTS
#endif

using System;
using System.Threading;
using BSLib;

namespace GKCore.SingleInstance
{
#if IPC_SUPPORTS
    using System.Runtime.Remoting;
    using System.Runtime.Remoting.Channels;
    using System.Runtime.Remoting.Channels.Ipc;
#endif

    /// <summary>
    /// Represents an object used to check for a previous instance of an application, and sending messages to it.
    /// </summary>
    public class SingleInstanceTracker : BaseObject
    {
        private readonly bool fIsFirstInstance;
        private readonly SingleInstanceProxy fProxy;

        #if IPC_SUPPORTS
        private IChannel fIpcChannel;
        private Mutex fSingleInstanceMutex;
        #endif

        /// <summary>
        /// Gets a value indicating whether this instance of the application is the first instance.
        /// </summary>
        public bool IsFirstInstance
        {
            get { return fIsFirstInstance; }
        }

        /// <summary>
        /// Gets the single instance enforcer (the first instance of the application) which would receive messages.
        /// </summary>
        public ISingleInstanceEnforcer Enforcer
        {
            get { return fProxy.Enforcer; }
        }

        /// <summary>
        /// Instantiates a new SingleInstanceTracker object.
        /// When using this constructor overload and enforcerRetriever is null, the SingleInstanceTracker object can only be used to determine whether the application is already running.
        /// </summary>
        /// <param name="name">The unique name used to identify the application.</param>
        /// <param name="enforcerRetriever">The method which would be used to retrieve an ISingleInstanceEnforcer object when instantiating the new object.</param>
        /// <exception cref="System.ArgumentNullException">name is null or empty.</exception>
        /// <exception cref="SingleInstancing.SingleInstancingException">A general error occurred while trying to instantiate the SingleInstanceInteractor. See InnerException for more details.</exception>
        public SingleInstanceTracker(string name, SingleInstanceEnforcerRetriever enforcerRetriever)
        {
            if (string.IsNullOrEmpty(name))
                throw new ArgumentNullException("name", @"name cannot be null or empty.");

            try
            {
                // Do not attempt to construct the IPC channel if there is no need for messages
                if (enforcerRetriever != null)
                {
                    #if IPC_SUPPORTS

                    fSingleInstanceMutex = new Mutex(true, name, out fIsFirstInstance);

                    const string proxyObjectName = "SingleInstanceProxy";
                    string proxyUri = "ipc://" + name + "/" + proxyObjectName;

                    // If no previous instance was found, create a server channel which will provide the proxy to the first created instance
                    if (fIsFirstInstance)
                    {
                        // Create an IPC server channel to listen for SingleInstanceProxy object requests
                        fIpcChannel = new IpcServerChannel(name);
                        // Register the channel and get it ready for use
                        ChannelServices.RegisterChannel(fIpcChannel, false);
                        // Register the service which gets the SingleInstanceProxy object, so it can be accessible by IPC client channels
                        RemotingConfiguration.RegisterWellKnownServiceType(typeof(SingleInstanceProxy), proxyObjectName, WellKnownObjectMode.Singleton);

                        // Attempt to retrieve the enforcer from the delegated method
                        ISingleInstanceEnforcer enforcer = enforcerRetriever();
                        // Validate that an enforcer object was returned
                        if (enforcer == null)
                            throw new InvalidOperationException("The method delegated by the enforcerRetriever argument returned null. The method must return an ISingleInstanceEnforcer object.");

                        // Create the first proxy object
                        fProxy = new SingleInstanceProxy(enforcer);
                        // Publish the first proxy object so IPC clients requesting a proxy would receive a reference to it
                        RemotingServices.Marshal(fProxy, proxyObjectName);
                    }
                    else
                    {
                        // Create an IPC client channel to request the existing SingleInstanceProxy object.
                        fIpcChannel = new IpcClientChannel();
                        // Register the channel and get it ready for use
                        ChannelServices.RegisterChannel(fIpcChannel, false);

                        // Retreive a reference to the proxy object which will be later used to send messages
                        fProxy = (SingleInstanceProxy)Activator.GetObject(typeof(SingleInstanceProxy), proxyUri);

                        // Notify the first instance of the application that a new instance was created
                        fProxy.Enforcer.OnNewInstanceCreated(new EventArgs());
                    }
                    
                    #else

                    fIsFirstInstance = IpcFake.CreateMutex(name, true);

                    if (fIsFirstInstance) {
                        // Attempt to retrieve the enforcer from the delegated method
                        ISingleInstanceEnforcer enforcer = enforcerRetriever();
                        // Validate that an enforcer object was returned
                        if (enforcer == null)
                            throw new InvalidOperationException("The method delegated by the enforcerRetriever argument returned null. The method must return an ISingleInstanceEnforcer object.");

                        // Create the first proxy object
                        fProxy = new SingleInstanceProxy(enforcer);

                        IpcFake.StartServer(enforcer);
                    } else {
                    }

                    #endif
                }
            }
            catch (Exception ex)
            {
                throw new SingleInstancingException("Failed to instantiate a new SingleInstanceTracker object. See InnerException for more details.", ex);
            }
        }

        /// <summary>
        /// Releases all unmanaged resources used by the object, and potentially releases managed resources.
        /// </summary>
        /// <param name="disposing">true to dispose of managed resources; otherwise false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                #if IPC_SUPPORTS

                if (fSingleInstanceMutex != null) {
                    fSingleInstanceMutex.Close();
                    fSingleInstanceMutex = null;
                }

                if (fIpcChannel != null) {
                    ChannelServices.UnregisterChannel(fIpcChannel);
                    fIpcChannel = null;
                }

                #else

                IpcFake.StopServer();
                IpcFake.ReleaseAllMutexes();

                #endif
            }
            base.Dispose(disposing);
        }

        /// <summary>
        /// Sends a message to the first instance of the application.
        /// </summary>
        /// <param name="message">The message to send to the first instance of the application. The message must be serializable.</param>
        /// <exception cref="System.InvalidOperationException">The object was constructed with the SingleInstanceTracker(string name) constructor overload,
        /// or with the SingleInstanceTracker(string name, SingleInstanceEnforcerRetriever enforcerRetriever) constructor overload, with enforcerRetriever set to null.</exception>
        /// <exception cref="SingleInstancing.SingleInstancingException">The SingleInstanceInteractor has failed to send the message to the first application instance.
        /// The first instance might have terminated.</exception>
        public void SendMessageToFirstInstance(object message)
        {
            #if IPC_SUPPORTS

            if (fIpcChannel == null)
                throw new InvalidOperationException("The object was constructed with the SingleInstanceTracker(string name) constructor overload, or with the SingleInstanceTracker(string name, SingleInstanceEnforcerRetriever enforcerRetriever) constructor overload, with enforcerRetriever set to null, thus you cannot send messages to the first instance.");

            try {
                fProxy.Enforcer.OnMessageReceived(new MessageEventArgs(message));
            } catch (Exception ex) {
                throw new SingleInstancingException("Failed to send message to the first instance of the application. The first instance might have terminated.", ex);
            }

            #else

            try {
                string[] args = message as string[];
                if (args.Length == 0 || string.IsNullOrEmpty(args[0])) {
                    IpcFake.Send(AppMessage.RestoreWindow, 0, false);
                } else {
                    IpcFake.SendMessage(IpcFake.CmdSendArgs, args);
                }
            } catch (Exception ex) {
                Logger.WriteError("SingleInstanceTracker.SendMessageToFirstInstance.2()", ex);
            }

            #endif
        }
    }
}
