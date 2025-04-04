/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2023 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using GDModel;
using GKCore.Design.Graphics;
using GKCore.Interfaces;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PortraitsCache : BaseObject
    {
        private static PortraitsCache fInstance = null;

        public static PortraitsCache Instance
        {
            get {
                if (fInstance == null) fInstance = new PortraitsCache();
                return fInstance;
            }
        }

        private readonly Dictionary<string, IImage> fMemoryCache;

        private PortraitsCache()
        {
            fMemoryCache = new Dictionary<string, IImage>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                foreach (KeyValuePair<string, IImage> pair in fMemoryCache) {
                    pair.Value.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        public IImage GetImage(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (context == null || iRec == null) return null;

            IImage result;

            // get multimedia UID
            string imageUID = context.GetPrimaryBitmapUID(iRec);

            // portrait doesn't define for individual
            if (string.IsNullOrEmpty(imageUID)) return null;

            string cachedFile = BaseContext.GetCachedImageFilename(imageUID);

            // check in-memory cache
            if (fMemoryCache.TryGetValue(cachedFile, out result)) {
                return result;
            }

            // in-memory cache doesn't contain image
            // check cache folder by multimedia UID
            if (File.Exists(cachedFile)) {
                result = AppHost.GfxProvider.LoadImage(cachedFile);
            }

            // if cache doesn't contain the image, then load and save it to cache
            if (result == null) {
                result = context.GetPrimaryBitmap(iRec, -1, -1, true);

                // save image to cache
                if (result != null) {
                    AppHost.GfxProvider.SaveImage(result, cachedFile);
                }
            }

            // put new image from disk's cache or storage to memory cache
            if (result != null) {
                fMemoryCache.Add(cachedFile, result);
            }

            // return result image
            return result;
        }

        public void RemoveObsolete(string imageUID)
        {
            if (string.IsNullOrEmpty(imageUID)) return;

            try {
                string cachedFile = BaseContext.GetCachedImageFilename(imageUID);

                if (fMemoryCache.ContainsKey(cachedFile)) {
                    fMemoryCache.Remove(cachedFile);
                }

                if (File.Exists(cachedFile)) {
                    File.Delete(cachedFile);
                }
            } catch (Exception ex) {
                Logger.WriteError("PortraitsCache.RemoveObsolete()", ex);
            }
        }
    }
}
