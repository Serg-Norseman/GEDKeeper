#if NETCOREAPP

using System;
using Eto.Drawing;
using OpenCvSharp;

namespace GKVisionPlugin
{
    internal static class BitmapConverter
    {
        public static unsafe Mat ToMat(this Bitmap bitmap)
        {
            if (bitmap == null)
                throw new ArgumentNullException(nameof(bitmap));

            using (var data = bitmap.Lock()) {
                var matType = MatType.CV_8UC(data.BytesPerPixel);
                Mat dst = new Mat(bitmap.Height, bitmap.Width, matType);

                switch (data.BytesPerPixel) {
                    case 3:
                        Format24bppRgb(data, bitmap.Height, bitmap.Width, dst);
                        break;

                    case 4:
                        Format32bppRgb(data, bitmap.Height, bitmap.Width, dst);
                        break;

                    default:
                        throw new ArgumentException("Format is not supported");
                }

                return dst;
            }
        }

        private static unsafe void Format24bppRgb(BitmapData bd, int h, int w, Mat dst)
        {
            var srcStep = bd.ScanWidth;
            var dstStep = dst.Step();
            if (dstStep == srcStep && !dst.IsSubmatrix() && dst.IsContinuous()) {
                var dstData = dst.Data;
                var bytesToCopy = dst.DataEnd.ToInt64() - dstData.ToInt64();
                Buffer.MemoryCopy(bd.Data.ToPointer(), dstData.ToPointer(), bytesToCopy, bytesToCopy);
            } else {
                // Copy line bytes from src to dst for each line
                var sp = (byte*)bd.Data;
                var dp = (byte*)dst.Data;
                for (var y = 0; y < h; y++) {
                    Buffer.MemoryCopy(sp, dp, dstStep, dstStep);
                    sp += srcStep;
                    dp += dstStep;
                }
            }
        }

        private static unsafe void Format32bppRgb(BitmapData bd, int h, int w, Mat dst)
        {
            var srcStep = bd.ScanWidth;
            var dstStep = dst.Step();

            var srcPtr = (byte*)bd.Data.ToPointer();
            var dstPtr = (byte*)dst.Data.ToPointer();

            switch (dst.Channels()) {
                case 4:
                    if (!dst.IsSubmatrix() && dst.IsContinuous()) {
                        var dstData = dst.Data;
                        var bytesToCopy = dst.DataEnd.ToInt64() - dstData.ToInt64();
                        Buffer.MemoryCopy(srcPtr, dstPtr, bytesToCopy, bytesToCopy);
                    } else {
                        var sp = (byte*)bd.Data;
                        var dp = (byte*)dst.Data;
                        for (var y = 0; y < h; y++) {
                            Buffer.MemoryCopy(sp, dp, dstStep, dstStep);
                            sp += srcStep;
                            dp += dstStep;
                        }
                    }
                    break;

                case 3:
                    for (var y = 0; y < h; y++) {
                        for (var x = 0; x < w; x++) {
                            dstPtr[y * dstStep + x * 3 + 0] = srcPtr[y * srcStep + x * 4 + 0];
                            dstPtr[y * dstStep + x * 3 + 1] = srcPtr[y * srcStep + x * 4 + 1];
                            dstPtr[y * dstStep + x * 3 + 2] = srcPtr[y * srcStep + x * 4 + 2];
                        }
                    }
                    break;

                default:
                    throw new ArgumentException("Invalid nChannels");
            }
        }
    }
}

#endif
