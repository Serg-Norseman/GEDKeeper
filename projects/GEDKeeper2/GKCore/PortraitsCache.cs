/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.IO;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PortraitsCache
    {
        private static PortraitsCache fInstance = null;


        public static PortraitsCache Instance
        {
            get {
                if (fInstance == null) fInstance = new PortraitsCache();
                return fInstance;
            }
        }


        private PortraitsCache()
        {
        }

        public Image GetImage(IBaseContext context, GEDCOMIndividualRecord iRec)
        {
            if (context == null || iRec == null) return null;

            Image result = null;

            // calculate file reference hash
            string imageHash = GKUtils.GetFileReferenceHash(null);

            // check cache folder by fileRef hash
            string cachedFile = GKUtils.GetCachePath() + imageHash + ".bmp";
            if (File.Exists(cachedFile)) {
                
            }

            // if cache isn't contains image, then load and save to cache
            if (result == null) {
                result = context.GetPrimaryBitmap(iRec, -1, -1, true);
            }

            // return result image
            return result;
        }
    }
}
