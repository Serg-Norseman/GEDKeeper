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

// TODO: When building, runtime is not copied to the plugins folder
// TODO: Remove Accord support
// TODO: GKv3 plugin

//#define ACCORD_CV
#define OPEN_CV

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Media;
using GKCore.Utilities;
using GKUI.Platform.Handlers;

namespace GKVisionPlugin
{
#if !NETCOREAPP
    using System.Drawing;
#else
    using Eto.Drawing;
#endif

#if ACCORD_CV
    using Accord.Vision.Detection;
    using Accord.Vision.Detection.Cascades;
#elif OPEN_CV
    using System.IO.Compression;
    using OpenCvSharp;
#if !NETCOREAPP
    using OpenCvSharp.Extensions;
#endif
    using OpenCvSharp.Face;
#endif

    internal class GKComputerVision : IComputerVision
    {
        public string ExecPath { get; }

        public GKComputerVision()
        {
            Assembly asm = this.GetType().Assembly;
            Module[] mods = asm.GetModules();
            string fn = mods[0].FullyQualifiedName;
            ExecPath = Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;

            InitExt();
        }

        public bool HasSubject(string strInfo)
        {
            return HasSubjectInt(strInfo);
        }

        public CVSubject[] DetectSubjects(IImage image)
        {
            if (image == null) return null;
            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return null;

            return DetectSubjectsInt(sdImg);
        }

        public void TrainFace(IImage image, int label, string strInfo)
        {
            if (image == null) return;
            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return;

            TrainFaceInt(sdImg, label, strInfo);
        }

        public string PredictFace(IImage image, out float confidence)
        {
            confidence = 0.0f;
            if (image == null) return null;
            var sdImg = ((ImageHandler)image).Handle as Bitmap;
            if (sdImg == null) return null;

            return PredictFaceInt(sdImg, out confidence);
        }

        public void Save()
        {
            try {
                SaveModelInt();
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.Save()", ex);
            }

            try {
                string[] strings = fTrainHistory.ToArray();

                var fileName = GetHistoryFileName();
                using (var writer = new StreamWriter(fileName)) {
                    string content = YamlHelper.Serialize(strings);
                    writer.Write(content);
                    writer.Flush();
                }
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.SaveHistory()", ex);
            }
        }

        public void Restore()
        {
            try {
                RestoreModelInt();
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.Restore()", ex);
            }

            try {
                var fileName = GetHistoryFileName();
                if (!File.Exists(fileName)) return;

                string[] strings;
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    strings = YamlHelper.Deserialize<string[]>(content);

                    foreach (string s in strings) {
                        fTrainHistory.Add(s);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.RestoreHistory()", ex);
            }
        }

        private HashSet<string> fTrainHistory = new HashSet<string>();

        private static string GetHistoryFileName()
        {
            return Path.Combine(AppHost.Instance.GetAppDataPath(), "cvhist.yaml");
        }

        private static string GetModelFileName()
        {
            return Path.Combine(AppHost.Instance.GetAppDataPath(), "cvmodel.yaml");
        }

        public void AddMediaLink(string linkSign)
        {
            fTrainHistory.Add(linkSign);
        }

        public bool HasMediaLink(string linkSign)
        {
            return fTrainHistory.Contains(linkSign);
        }

        #region OpenCV internals
#if OPEN_CV
        private const string HaarCascadeFileName = "haarcascade_frontalface_default.xml";
        private FaceRecognizer fFaceRecognizer;
        private CascadeClassifier fCascadeClassifier;

        private void InitExt()
        {
            try {
                fFaceRecognizer = LBPHFaceRecognizer.Create();
            } catch (Exception ex) {
                Logger.WriteError("GKComputerVision.InitExt()", ex);
            }
        }

        private void CheckHaarCascadeData()
        {
            string cascadeFile = Path.Combine(ExecPath, HaarCascadeFileName);
            if (File.Exists(cascadeFile)) return;

            string gzName = "GKVisionPlugin.Resources.haarcascade.gz";
            Assembly assembly = typeof(GKComputerVision).Assembly;
            Stream resStream = assembly.GetManifestResourceStream(gzName);
            using (var gs = new GZipStream(resStream, CompressionMode.Decompress)) {
                using (var outStream = new FileStream(cascadeFile, FileMode.Create)) {
                    byte[] tmp = new byte[1024 * 256];
                    int r;
                    while ((r = gs.Read(tmp, 0, tmp.Length)) > 0) {
                        outStream.Write(tmp, 0, r);
                    }
                    outStream.Flush();
                }
            }
        }

        private void CheckClassifier()
        {
            if (fCascadeClassifier == null) {
                CheckHaarCascadeData();
                string cascadeFile = Path.Combine(ExecPath, HaarCascadeFileName);
                fCascadeClassifier = new CascadeClassifier(cascadeFile);
            }
        }

        private bool HasSubjectInt(string strInfo)
        {
            if (fFaceRecognizer == null || fFaceRecognizer.Empty) return false;

            int[] labels = fFaceRecognizer.GetLabelsByString(strInfo);
            return (labels != null && labels.Length > 0);
        }

        private CVSubject[] DetectSubjectsInt(Bitmap image)
        {
            CheckClassifier();

            Rect rtInside = new Rect(0, 0, image.Width, image.Height);
            rtInside.Inflate(-10, -10);

            using (var frameMat = BitmapConverter.ToMat(image)) {
                var rects = fCascadeClassifier.DetectMultiScale(frameMat, 1.1, 5, HaarDetectionTypes.ScaleImage, new OpenCvSharp.Size(30, 30));

                var result = new CVSubject[rects.Length];
                if (rects.Length > 0) {
                    for (int i = 0; i < rects.Length; i++) {
                        var rtFace = rects[i];

                        var rtPortrait = rtFace;
                        rtPortrait.Inflate((int)(rtFace.Width * 0.3), (int)(rtFace.Height * 0.6));
                        rtPortrait = Rect.Intersect(rtPortrait, rtInside);

                        result[i] = new CVSubject(
                            new ExtRect(rtFace.Left, rtFace.Top, rtFace.Width, rtFace.Height),
                            new ExtRect(rtPortrait.Left, rtPortrait.Top, rtPortrait.Width, rtPortrait.Height));
                    }
                }

                return result;
            }
        }

        private void TrainFaceInt(Bitmap image, int label, string strInfo)
        {
            if (fFaceRecognizer == null) return;

            CheckClassifier();

            using (Mat frameMat = BitmapConverter.ToMat(image)) {
                var rects = fCascadeClassifier.DetectMultiScale(frameMat, 1.1, 5, HaarDetectionTypes.FindBiggestObject, new OpenCvSharp.Size(30, 30));
                if (rects.Length <= 0) return;

                var rtFace = rects[0];
                using (Mat faceMat = new Mat(frameMat, rtFace))
                using (Mat dstMat = new Mat()) {
                    Cv2.CvtColor(faceMat, dstMat, ColorConversionCodes.BGR2GRAY);
                    if (fFaceRecognizer.Empty) {
                        fFaceRecognizer.Train(new Mat[] { dstMat }, new int[] { label });
                    } else {
                        fFaceRecognizer.Update(new Mat[] { dstMat }, new int[] { label });
                    }
                    fFaceRecognizer.SetLabelInfo(label, strInfo);
                }
            }
        }

        private string PredictFaceInt(Bitmap image, out float confidence)
        {
            if (fFaceRecognizer == null || fFaceRecognizer.Empty) {
                confidence = 0.0f;
                return null;
            }

            using (Mat frameMat = BitmapConverter.ToMat(image))
            using (Mat dstMat = new Mat()) {
                Cv2.CvtColor(frameMat, dstMat, ColorConversionCodes.BGR2GRAY);
                fFaceRecognizer.Predict(dstMat, out int label, out double ocvConfidence);
                confidence = (100.0f - (float)ocvConfidence) / 100.0f;
                return fFaceRecognizer.GetLabelInfo(label);
            }
        }

        private void SaveModelInt()
        {
            if (fFaceRecognizer == null) return;

            string fileName = GetModelFileName();
            fFaceRecognizer.Write(fileName);
        }

        private void RestoreModelInt()
        {
            if (fFaceRecognizer == null) return;

            string fileName = GetModelFileName();
            if (File.Exists(fileName))
                fFaceRecognizer.Read(fileName);
        }
#endif
        #endregion

        #region Accord.NET internals
#if ACCORD_CV

        private void InitExt()
        {
        }

        private ExtRect[] DetectFacesInt(Bitmap image, bool portraitMode)
        {
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

            return result;
        }

#endif
        #endregion
    }
}

