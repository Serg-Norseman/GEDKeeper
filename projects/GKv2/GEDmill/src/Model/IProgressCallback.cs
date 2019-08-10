/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;

namespace GEDmill.Model
{
    /// <summary>
    /// This defines an interface which can be implemented by UI elements
    /// which indicate the progress of a long operation.
    /// (See ProgressWindow for a typical implementation)
    /// </summary>
    public interface IProgressCallback
    {
        /// <summary>
        /// Call this method from the worker thread to initialize
        /// the progress callback.
        /// </summary>
        /// <param name="minimum">The minimum value in the progress range (e.g. 0)</param>
        /// <param name="maximum">The maximum value in the progress range (e.g. 100)</param>
        void Begin(int minimum, int maximum);

        /// <summary>
        /// Call this method from the worker thread to initialize
        /// the progress callback, without setting the range
        /// </summary>
        void Begin();

        /// <summary>
        /// Call this method from the worker thread to reset the range in the progress callback
        /// </summary>
        /// <param name="minimum">The minimum value in the progress range (e.g. 0)</param>
        /// <param name="maximum">The maximum value in the progress range (e.g. 100)</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void SetRange(int minimum, int maximum);

        /// <summary>
        /// Call this method from the worker thread to update the progress text.
        /// </summary>
        /// <param name="text">The progress text to display</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void SetText(String text);

        /// <summary>
        /// Call this method from the worker thread to increase the progress counter by a specified value.
        /// </summary>
        /// <param name="val">The amount by which to increment the progress indicator</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void StepTo(int val);

        /// <summary>
        /// Call this method from the worker thread to step the progress meter to a particular value.
        /// </summary>
        /// <param name="val">The value to which to step the meter</param>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void Increment(int val);

        /// <summary>
        /// If this property is true, then you should abort work
        /// </summary>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        bool IsAborting
        {
            get;
        }

        /// <summary>
        /// Call this method from the worker thread to finalize the progress meter
        /// </summary>
        /// <remarks>You must have called one of the Begin() methods prior to this call.</remarks>
        void End(ThreadError thread_error);
    }
}
