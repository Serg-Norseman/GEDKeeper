/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public struct CharsetResult
    {
        public string Charset;
        public float Confidence;

        public CharsetResult(string charset, float confidence)
        {
            Charset = charset;
            Confidence = confidence;
        }
    }
}
