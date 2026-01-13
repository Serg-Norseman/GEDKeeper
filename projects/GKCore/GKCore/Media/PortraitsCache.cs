/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using BSLib;
using GDModel;
using GKCore.Design.Graphics;

namespace GKCore.Media
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

        public IImage GetImage(BaseContext context, GDMIndividualRecord iRec)
        {
            if (context == null || iRec == null) return null;

            IImage result;

            // get multimedia UID
            string imageUID = context.GetPrimaryBitmapUID(iRec);

            // portrait doesn't define for individual
            if (string.IsNullOrEmpty(imageUID)) return null;

            string cachedFile = BaseContext.GetCachedImageFilename(imageUID, 0);

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
                string cachedFile = BaseContext.GetCachedImageFilename(imageUID, 0);

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
