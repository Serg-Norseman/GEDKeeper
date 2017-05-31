/**
 * Original SingleInstancingWithIpc code by Shy Agam
 * (http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution)
 */
using System;

namespace GKCore.SingleInstance
{
    /// <summary>
    /// Provides data for the SingleInstancing.ISingleInstanceEnforcer.OnMessageReceived method.
    /// </summary>
    public class MessageEventArgs : EventArgs
    {
        /// <summary>
        /// Gets the message sent to the first instance of the application.
        /// </summary>
        public readonly object Message;

        /// <summary>
        /// Instantiates a new MessageEventArgs object.
        /// </summary>
        /// <param name="message">The message to pass to the first running instance of the application.</param>
        /// <exception cref="System.ArgumentNullException">message is null.</exception>
        public MessageEventArgs(object message)
        {
            if (message == null)
                throw new ArgumentNullException("message", @"message cannot be null.");

            Message = message;
        }
    }


    /// <summary>
    /// Provides methods which a single instance application can use to in order to be respond to any new instance of it.
    /// </summary>
    public interface ISingleInstanceEnforcer
    {
        /// <summary>
        /// Handles messages received from a new instance of the application.
        /// </summary>
        /// <param name="e">The EventArgs object holding information about the event.</param>
        void OnMessageReceived(MessageEventArgs e);

        /// <summary>
        /// Handles a creation of a new instance of the application.
        /// </summary>
        /// <param name="e">The EventArgs object holding information about the event.</param>
        void OnNewInstanceCreated(EventArgs e);
    }


    /// <summary>
    /// Represents the method which would be used to retrieve an ISingleInstanceEnforcer object when instantiating a SingleInstanceTracker object.
    /// If the method returns null, the SingleInstanceTracker's constructor will throw an exception.
    /// </summary>
    /// <returns>An ISingleInstanceEnforcer object which would receive messages.</returns>
    public delegate ISingleInstanceEnforcer SingleInstanceEnforcerRetriever();


    /// <summary>
    /// Represents errors occured while trying to enforce single application instancing.
    /// </summary>
    public class SingleInstancingException : Exception
    {
        /// <summary>
        /// Instantiates a new SingleInstancingException object.
        /// </summary>
        /// <param name="message">The message that describes the error.</param>
        /// <param name="inner">The exception that is the cause of the current exception, or a null reference (Nothing in Visual Basic) if no inner exception is specified.</param>
        public SingleInstancingException(string message, Exception inner) : base(message, inner)
        {
        }
    }
}
