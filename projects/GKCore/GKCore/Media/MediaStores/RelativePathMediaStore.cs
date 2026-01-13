/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;

namespace GKCore.Media
{
    public sealed class RelativePathMediaStore : PlainMediaStore
    {
        public override MediaStoreType StoreType { get { return MediaStoreType.mstRelativeReference; } }


        public RelativePathMediaStore(BaseContext baseContext) : base(baseContext)
        {
        }

        public RelativePathMediaStore(BaseContext baseContext, string fileName) : base(baseContext, fileName)
        {
            fAbsoluteFileName = Path.GetFullPath(Path.Combine(baseContext.GetTreePath(), fileName));
        }
    }
}
