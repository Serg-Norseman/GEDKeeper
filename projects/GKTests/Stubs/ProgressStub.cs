/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System;
using GKCore.Design;
using GKCore.Interfaces;

namespace GKTests.Stubs
{
    internal class ProgressStub : IProgressController
    {
        public bool Enabled { get; set; }
        public void Activate() { }
        public void Close() { }
        public string Title { get; set; }
        public void Dispose() { }
        public void SetToolTip(object component, string toolTip) { }
        public object GetControl(string controlName) { return null; }
        public bool ShowModalX(IView owner) { return true; }


        public bool IsCanceled { get { return false; } }

        public void Begin(int maximum, bool cancelable) { }
        public void Begin(string title, int max, bool cancelable = false) { }
        public void End() { }
        public void End(ThreadError threadError) { }
        public void SetText(string text) { }
        public void Increment(int val) { }
        public void StepTo(int val) { }
        public void InvokeEx(Action action) { }
    }
}
