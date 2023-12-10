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
using System.Collections.Generic;

namespace GKCore.Design
{
    public sealed class ControlsManager
    {
        private static readonly Dictionary<Type, Type> fHandlerTypes = new Dictionary<Type, Type>();

        private readonly Dictionary<object, IControl> fObjHandlers;
        private readonly Dictionary<string, IControl> fStrHandlers;
        private readonly IView fView;

        public ControlsManager(IView view)
        {
            fView = view;
            fObjHandlers = new Dictionary<object, IControl>();
            fStrHandlers = new Dictionary<string, IControl>();
        }

        public T GetControl<T>(object control) where T : class, IControl
        {
            IControl handler;
            if (control is T) {
                handler = (T)control;
            } else {
                if (!fObjHandlers.TryGetValue(control, out handler)) {
                    Type controlType = control.GetType();
                    Type handlerType;
                    if (fHandlerTypes.TryGetValue(controlType, out handlerType)) {
                        handler = (IControl)Activator.CreateInstance(handlerType, control);
                        fObjHandlers.Add(control, handler);
                    } else {
                        throw new Exception(string.Format("handler type for `{0}` not found", controlType.Name));
                    }
                }
            }
            return (T)handler;
        }

        public T GetControl<T>(string controlName) where T : class, IControl
        {
            IControl control;
            if (!fStrHandlers.TryGetValue(controlName, out control)) {
                if (fView == null)
                    throw new ArgumentException("View is null");

                var ctlObj = fView.GetControl(controlName);

                if (ctlObj == null)
                    throw new ArgumentException("Field not found");

                control = GetControl<T>(ctlObj);
                fStrHandlers.Add(controlName, control);
            }
            return (T)control;
        }

        public static Type GetControlHandlerType<T>(Type controlType) where T : class
        {
            Type handlerType;
            return fHandlerTypes.TryGetValue(controlType, out handlerType) ? handlerType : null;
        }

        public static void RegisterHandlerType(Type controlType, Type handlerType)
        {
            if (fHandlerTypes.ContainsKey(controlType)) {
                fHandlerTypes.Remove(controlType);
            }
            fHandlerTypes.Add(controlType, handlerType);
        }
    }
}
