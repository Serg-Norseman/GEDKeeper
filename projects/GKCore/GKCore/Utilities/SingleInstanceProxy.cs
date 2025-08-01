/**
 * Original SingleInstancingWithIpc code by Shy Agam
 * (http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution)
 */
using System;

namespace GKCore.Utilities
{
    /// <summary>
    /// Provides a proxy to communicate with the first instance of the application.
    /// </summary>
    internal class SingleInstanceProxy : MarshalByRefObject
    {
        private readonly ISingleInstanceEnforcer fEnforcer;

        /// <summary>
        /// Gets or sets the enforcer (first instance of the application) which will receive messages from the new instances of the application.
        /// </summary>
        public ISingleInstanceEnforcer Enforcer
        {
            get { return fEnforcer; }
        }

        /// <summary>
        /// Instantiates a new SingleInstanceProxy object.
        /// </summary>
        /// <param name="enforcer">The enforcer (first instance of the application) which will receive messages from the new instances of the application.</param>
        /// <exception cref="System.ArgumentNullException">enforcer is null.</exception>
        public SingleInstanceProxy(ISingleInstanceEnforcer enforcer)
        {
            if (enforcer == null)
                throw new ArgumentNullException(nameof(enforcer), @"enforcer cannot be null.");

            fEnforcer = enforcer;
        }

        /// <summary>
        /// Obtains a lifetime service object to control the lifetime policy for this instance.
        /// </summary>
        /// <returns>An object of type System.Runtime.Remoting.Lifetime.ILease used to control the lifetime policy for this instance. This is the current lifetime service object for this instance if one exists; otherwise, a new lifetime service object initialized to the value of the System.Runtime.Remoting.Lifetime.LifetimeServices.LeaseManagerPollTime property.</returns>
        /// <exception cref="System.Security.SecurityException">The immediate caller does not have infrastructure permission.</exception>
        public override object InitializeLifetimeService()
        {
            return null;
        }
    }
}
