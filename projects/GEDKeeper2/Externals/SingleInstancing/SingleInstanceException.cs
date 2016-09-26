/**
 * Original SingleInstancingWithIpc code by Shy Agam
 * (http://www.codeproject.com/Articles/19682/A-Pure-NET-Single-Instance-Application-Solution)
 */
using System;
using System.Runtime.Serialization;

namespace Externals.SingleInstancing
{
    /// <summary>
    /// Represents errors occured while trying to enforce single application instancing.
    /// </summary>
    [Serializable]
    public class SingleInstancingException : Exception
    {
        /// <summary>
        /// Instantiates a new SingleInstancingException object.
        /// </summary>
        public SingleInstancingException() { }

        /// <summary>
        /// Instantiates a new SingleInstancingException object.
        /// </summary>
        /// <param name="message">The message that describes the error.</param>
        public SingleInstancingException(string message)
            : base(message) { }

        /// <summary>
        /// Instantiates a new SingleInstancingException object.
        /// </summary>
        /// <param name="message">The message that describes the error.</param>
        /// <param name="inner">The exception that is the cause of the current exception, or a null reference (Nothing in Visual Basic) if no inner exception is specified.</param>
        public SingleInstancingException(string message, Exception inner)
            : base(message, inner) { }
        
        /// <summary>
        /// Instantiates a new SingleInstancingException object with serialized data.
        /// </summary>
        /// <param name="info">The System.Runtime.Serialization.SerializationInfo that holds the serialized object data about the exception being thrown.</param>
        /// <param name="context">The System.Runtime.Serialization.StreamingContext that contains contextual information about the source or destination.</param>
        protected SingleInstancingException(SerializationInfo info, StreamingContext context)
            : base(info, context) { }
    }
}
