/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore.Design
{
    public delegate void ProgressStart(IProgressController progressController);


    /// <summary>
    /// A data structure containing an nError code and a sMessage, for passing back from threads to their creator.
    /// </summary>
    public class ThreadError
    {
        // The error code
        public int Error;

        // The error message
        public string Message;


        public ThreadError(int error, string message)
        {
            Error = error;
            Message = message;
        }
    }


    /// <summary>
    /// This defines an interface which can be implemented by UI elements
    /// which indicate the progress of a long operation.
    /// </summary>
    public interface IProgressController
    {
        /// <summary>
        /// If this property is true, then you should abort work
        /// </summary>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        bool IsCanceled { get; }


        /// <summary>
        /// Call this method from the worker thread to initialize
        /// the progress callback.
        /// </summary>
        /// <param name="maximum">The maximum value in the progress range (e.g. 100)</param>
        /// <param name="cancelable">The flag that the process can be interrupted</param>
        void Begin(int maximum, bool cancelable);

        /// <summary>
        /// Call this method from the worker thread to initialize
        /// the progress callback.
        /// </summary>
        /// <param name="title">The status message</param>
        /// <param name="maximum">The maximum value in the progress range (e.g. 100)</param>
        /// <param name="cancelable">The flag that the process can be interrupted</param>
        void Begin(string title, int maximum, bool cancelable = false);

        /// <summary>
        /// Call this method from the worker thread to finalize the progress meter
        /// </summary>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void End();

        /// <summary>
        /// Call this method from the worker thread to finalize the progress meter
        /// </summary>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void End(ThreadError threadError);

        /// <summary>
        /// Call this method from the worker thread to update the progress text.
        /// </summary>
        /// <param name="text">The progress text to display</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void SetText(string text);

        /// <summary>
        /// Call this method from the worker thread to increase the progress counter by a specified value.
        /// </summary>
        /// <param name="value">The amount by which to increment the progress indicator</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void StepTo(int value);

        /// <summary>
        /// Call this method from the worker thread to step the progress meter to a particular value.
        /// </summary>
        /// <param name="val">The value to which to step the meter</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void Increment(int value = 1);

        void InvokeEx(Action action);
    }


    /// <summary>
    /// This defines an interface which can be implemented by UI elements
    /// which indicate the progress of a long operation.
    /// </summary>
    public interface IProgressDialog : IProgressController, ICommonDialog
    {
        ThreadError ThreadError { get; set; }
    }
}
