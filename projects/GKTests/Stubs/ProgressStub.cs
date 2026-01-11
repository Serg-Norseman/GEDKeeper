// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using GKCore.Design;

namespace GKTests.Stubs
{
    internal class ProgressStub : IProgressDialog
    {
        public bool Enabled { get; set; }
        public bool Visible { get; set; }
        public void Activate() { }
        public void Close() { }
        public void Dispose() { }
        public void SetToolTip(object component, string toolTip) { }
        public object GetControl(string controlName) { return null; }
        T IView.GetCoreControl<T>(string controlName) { return default(T); }
        public bool ShowModalX(IView owner) { return true; }


        public bool IsCanceled { get { return false; } }

        public ThreadError ThreadError { get { return null; } set { } }

        public void Begin(int maximum, bool cancelable) { }
        public void Begin(string title, int max, bool cancelable = false) { }
        public void End() { }
        public void End(ThreadError threadError) { }
        public void SetText(string text) { }
        public void Increment(int val) { }
        public void StepTo(int val) { }
        public void InvokeEx(Action action) { }
        public void SetTitle(string value) { }
    }
}
