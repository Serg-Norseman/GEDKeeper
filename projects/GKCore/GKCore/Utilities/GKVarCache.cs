/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;

namespace GKCore.Utilities
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKVarCache<TKey, TValue>
    {
        private readonly TValue fDefaultValue;
        private readonly Dictionary<TKey, TValue> fDictionary;

        public TValue this[TKey key]
        {
            get {
                TValue value;
                if (fDictionary.TryGetValue(key, out value)) {
                    return value;
                } else {
                    return fDefaultValue;
                }
            }
            set {
                fDictionary[key] = value;
            }
        }

        public GKVarCache(TValue defaultValue = default(TValue))
        {
            fDefaultValue = defaultValue;
            fDictionary = new Dictionary<TKey, TValue>();
        }

        public void Clear()
        {
            fDictionary.Clear();
        }
    }
}
