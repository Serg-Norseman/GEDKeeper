/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Design;

namespace GKUI.Platform;

public sealed class CLIProgress : IProgressController
{
    public ThreadError ThreadError { get; set; }


    public CLIProgress()
    {
        ThreadError = new ThreadError(1, "No error");
    }

    public bool IsCanceled
    {
        get {
            return false;
        }
    }

    public void Begin(int maximum, bool cancelable)
    {
    }

    public void Begin(string title, int maximum, bool cancelable = false)
    {
    }

    public void End()
    {
    }

    public void End(ThreadError threadError)
    {
    }

    public void SetText(string text)
    {
    }

    public void Increment(int value = 1)
    {
    }

    public void StepTo(int value)
    {
    }

    public void InvokeEx(Action action)
    {
    }
}
