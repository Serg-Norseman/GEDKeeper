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
    public interface IViewController : IDisposable
    {
        T GetControlHandler<T>(object control) where T : class, IControl;
        T GetControl<T>(string controlName) where T : class, IControl;
    }


    public interface IView : IBaseControl, IDisposable
    {
        IViewController Controller { get; }

        void Close();
        object GetControl(string controlName);
        void SetTitle(string value);
        void SetToolTip(object component, string toolTip);
    }
}
