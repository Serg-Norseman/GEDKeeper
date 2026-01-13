/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Locales;

namespace GKCore.Events
{
    public enum EventKind
    {
        ekEvent,
        ekFact
    }


    public enum EventTarget
    {
        etIndividual,
        etFamily,
        etAny
    }


    public sealed class PredefinedEvent
    {
        public LSID Name;
        public string Tag;
        public EventKind Kind;
        public EventTarget Target;
        public bool AcceptableEmpty;

        public PredefinedEvent(LSID name, string tag, EventKind kind, EventTarget target, bool acceptableEmpty = false)
        {
            Name = name;
            Tag = tag;
            Kind = kind;
            Target = target;
            AcceptableEmpty = acceptableEmpty;
        }
    }
}
