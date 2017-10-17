/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Collections.Generic;
using GKCommon;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NavigationStack<T> : BaseObject where T : class
    {
        private bool fNavBusy;
        private readonly Stack<T> fStackBackward;
        private readonly Stack<T> fStackForward;
        private T fCurrent;

        public bool Busy
        {
            get { return fNavBusy; }
        }

        public T Current
        {
            get { return fCurrent; }
            set { SetCurrent(value); }
        }

        private void SetCurrent(T value)
        {
            if (fCurrent == value) return;

            if (fCurrent != null)
            {
                fStackBackward.Push(fCurrent);
            }
            fCurrent = value;
            fStackForward.Clear();
        }

        public NavigationStack()
        {
            fStackBackward = new Stack<T>();
            fStackForward = new Stack<T>();
            fCurrent = default(T);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                // dummy
            }
            base.Dispose(disposing);
        }

        public object Back()
        {
            if (fCurrent != null)
            {
                fStackForward.Push(fCurrent);
            }
            fCurrent = (fStackBackward.Count > 0) ? fStackBackward.Pop() : null;
            return fCurrent;
        }

        public object Next()
        {
            if (fCurrent != null)
            {
                fStackBackward.Push(fCurrent);
            }
            fCurrent = (fStackForward.Count > 0) ? fStackForward.Pop() : null;
            return fCurrent;
        }

        public void Clear()
        {
            fStackBackward.Clear();
            fStackForward.Clear();
            fCurrent = null;
        }

        public void BeginNav()
        {
            fNavBusy = true;
        }

        public void EndNav()
        {
            fNavBusy = false;
        }

        public bool CanBackward()
        {
            return fStackBackward.Count > 0;
        }

        public bool CanForward()
        {
            return fStackForward.Count > 0;
        }
    }
}
