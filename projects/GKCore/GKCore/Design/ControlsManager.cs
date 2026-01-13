/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        private IView fView;

        public ControlsManager(IView view)
        {
            fView = view;
            fObjHandlers = new Dictionary<object, IControl>();
            fStrHandlers = new Dictionary<string, IControl>();
        }

        public void Clear()
        {
            fObjHandlers.Clear();
            fStrHandlers.Clear();
            fView = null;
        }

        public T GetControlHandler<T>(object control) where T : class, IControl
        {
            IControl handler;
            if (control is T ctlT) {
                handler = ctlT;
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

        public T GetCoreControl<T>(string controlName) where T : class, IControl
        {
            IControl control;
            if (!fStrHandlers.TryGetValue(controlName, out control)) {
                if (fView == null)
                    throw new ArgumentException("View is null");

                var ctlObj = fView.GetControl(controlName);

                if (ctlObj == null)
                    throw new ArgumentException($"Field `{controlName}` not found");

                control = GetControlHandler<T>(ctlObj);
                fStrHandlers.Add(controlName, control);
            }
            return (T)control;
        }

        public static void RegisterHandlerType(Type controlType, Type handlerType)
        {
            if (fHandlerTypes.ContainsKey(controlType)) {
                fHandlerTypes.Remove(controlType);
            }
            fHandlerTypes.Add(controlType, handlerType);
        }

        public static void ClearHandlerTypes()
        {
            fHandlerTypes.Clear();
        }
    }
}
