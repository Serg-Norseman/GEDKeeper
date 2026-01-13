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
    public interface IView : IBaseControl, IDisposable
    {
        void Close();
        object GetControl(string controlName);
        T GetCoreControl<T>(string controlName) where T : class, IControl;
        void SetTitle(string value);
        void SetToolTip(object component, string toolTip);
    }
}
