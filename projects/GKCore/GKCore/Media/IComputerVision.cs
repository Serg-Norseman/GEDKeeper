/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Media
{
    public sealed class CVSubject
    {
        public ExtRect Face;
        public ExtRect Portrait;

        public CVSubject(ExtRect face, ExtRect portrait)
        {
            Face = face;
            Portrait = portrait;
        }
    }


    public interface IComputerVision
    {
        bool HasSubject(string strInfo);

        CVSubject[] DetectSubjects(IImage image);
        void TrainFace(IImage image, int label, string strInfo);
        string PredictFace(IImage image, out float confidence);

        void Save();
        void Restore();

        void AddMediaLink(string linkSign);
        bool HasMediaLink(string linkSign);
    }
}
