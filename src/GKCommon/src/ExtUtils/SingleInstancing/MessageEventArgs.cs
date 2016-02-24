/**
 * Original SingleInstancingWithIpc code by Shy Agam
 * (http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution)
 */
using System;

namespace ExtUtils.SingleInstancing
{
	/// <summary>
	/// Provides data for the SingleInstancing.ISingleInstanceEnforcer.OnMessageReceived method.
	/// </summary>
	[Serializable]
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

			this.Message = message;
		}
	}
}