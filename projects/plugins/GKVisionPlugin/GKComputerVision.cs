/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

//#define ACCORD_CV
#define OPEN_CV

using System;
using System.Drawing;
using System.IO;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKUI.Platform.Handlers;

namespace GKVisionPlugin
{
#if ACCORD_CV
    using Accord.Vision.Detection;
    using Accord.Vision.Detection.Cascades;
#elif OPEN_CV
    using OpenCvSharp;
    using OpenCvSharp.Extensions;
    using OpenCvSharp.Face;
#endif

    internal class GKComputerVision : IComputerVision
    {
        public string ExecPath { get; set; }

        public ExtRect[] DetectFaces(IImage image, bool portraitMode)
        {
            if (image == null) return null;

            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return null;

#if ACCORD_CV
            var detector = new HaarObjectDetector(new FaceHaarCascade(), 30);
            detector.SearchMode = ObjectDetectorSearchMode.NoOverlap;
            detector.ScalingMode = ObjectDetectorScalingMode.SmallerToGreater;
            detector.ScalingFactor = 1.5f;
            detector.UseParallelProcessing = true;
            detector.Suppression = 2;

            Rectangle[] objects = detector.ProcessFrame(sdImg);
            var result = new ExtRect[objects.Length];

            for (int i = 0; i < objects.Length; i++) {
                Rectangle rectangle = objects[i];

                if (portraitMode) {
                    var w = rectangle.Width;
                    rectangle.X = Math.Max(0, rectangle.X - (int)(w * 0.333));
                    rectangle.Width = Math.Min(sdImg.Width, rectangle.Width + (int)(w * 0.666));

                    var h = rectangle.Height;
                    rectangle.Y = Math.Max(0, rectangle.Y - (int)(h * 0.5));
                    rectangle.Height = Math.Min(sdImg.Height, rectangle.Height + (int)((h * 0.666) + (h * 0.5)));
                }

                result[i] = new ExtRect(rectangle.Left, rectangle.Top, rectangle.Width, rectangle.Height);
            }
#elif OPEN_CV
            string cascadeFile = Path.Combine(ExecPath, "haarcascade_frontalface_default.xml");
            var cascadeClassifier = new CascadeClassifier(cascadeFile);

            var frameMat = BitmapConverter.ToMat(sdImg);
            var rects = cascadeClassifier.DetectMultiScale(frameMat, 1.1, 5, HaarDetectionTypes.ScaleImage, new OpenCvSharp.Size(30, 30));

            var result = new ExtRect[rects.Length];
            if (rects.Length > 0) {
                for (int i = 0; i < rects.Length; i++) {
                    var rectangle = rects[i];

                    if (portraitMode) {
                        var w = rectangle.Width;
                        rectangle.X = Math.Max(0, rectangle.X - (int)(w * 0.333));
                        rectangle.Width = Math.Min(sdImg.Width, rectangle.Width + (int)(w * 0.666));

                        var h = rectangle.Height;
                        rectangle.Y = Math.Max(0, rectangle.Y - (int)(h * 0.5));
                        rectangle.Height = Math.Min(sdImg.Height, rectangle.Height + (int)((h * 0.666) + (h * 0.5)));
                    }

                    result[i] = new ExtRect(rectangle.Left, rectangle.Top, rectangle.Width, rectangle.Height);
                }
            }
#else
            var result = new ExtRect[0];
#endif

            return result;
        }

        public void TrainFace(IImage image, GDMIndividualRecord indiRec)
        {
            if (image == null || indiRec == null) return;

            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return;

            var label = (int)indiRec.GetId();

#if ACCORD_CV
#elif OPEN_CV
            using (Mat frameMat = BitmapConverter.ToMat(sdImg))
            using (Mat dstMat = new Mat()) {
                Cv2.CvtColor(frameMat, dstMat, ColorConversionCodes.BGR2GRAY);
                if (fFaceRecognizer.Empty) {
                    fFaceRecognizer.Train(new Mat[] { dstMat }, new int[] { label });
                } else {
                    fFaceRecognizer.Update(new Mat[] { dstMat }, new int[] { label });
                }
                fFaceRecognizer.SetLabelInfo(label, indiRec.XRef);
            }
#else
#endif
        }

        public string PredictFace(IImage image)
        {
            if (image == null) return null;

            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return null;

#if ACCORD_CV
            return null;
#elif OPEN_CV
            if (fFaceRecognizer.Empty) return null;

            using (Mat frameMat = BitmapConverter.ToMat(sdImg))
            using (Mat dstMat = new Mat()) {
                Cv2.CvtColor(frameMat, dstMat, ColorConversionCodes.BGR2GRAY);
                fFaceRecognizer.Predict(dstMat, out int label, out double confidence);
                //return (confidence >= 80.0f) ? fFaceRecognizer.GetLabelInfo(label) : null;
                return fFaceRecognizer.GetLabelInfo(label);
            }
#else
            return null;
#endif
        }

#if ACCORD_CV
#elif OPEN_CV
        private FaceRecognizer fFaceRecognizer = LBPHFaceRecognizer.Create();
#else
#endif

        private string GetFileName()
        {
            return Path.Combine(AppHost.Instance.GetAppDataPath(), "ocv_model.yaml");
        }

        public void Save()
        {
#if OPEN_CV
            try {
                if (fFaceRecognizer == null) return;

                string fileName = GetFileName();
                fFaceRecognizer.Write(fileName);
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.Save()", ex);
            }
#endif
        }

        public void Restore()
        {
#if OPEN_CV
            try {
                if (fFaceRecognizer == null) return;

                string fileName = GetFileName();
                if (File.Exists(fileName))
                    fFaceRecognizer.Read(fileName);
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.Restore()", ex);
            }
#endif
        }
    }
}
