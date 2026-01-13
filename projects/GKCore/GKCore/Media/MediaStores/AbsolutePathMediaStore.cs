/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Media
{
    public sealed class AbsolutePathMediaStore : PlainMediaStore
    {
        public override MediaStoreType StoreType { get { return MediaStoreType.mstReference; } }


        public AbsolutePathMediaStore(BaseContext baseContext) : base(baseContext)
        {
        }

        public AbsolutePathMediaStore(BaseContext baseContext, string fileName) : base(baseContext, fileName)
        {
            fAbsoluteFileName = fileName;
        }
    }
}
