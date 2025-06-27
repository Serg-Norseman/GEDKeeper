using System;
using System.IO;

namespace GKCore
{
    class GKPrependStream : Stream
    {
        private readonly Stream fStream;
        private readonly byte[] fPrefix;
        private int fPrefixPos;

        public GKPrependStream(byte[] prefix, Stream stream)
        {
            fPrefix = prefix;
            fStream = stream;
        }


        public override void Flush()
        {
            fStream.Flush();
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            int bytesFromPrefix = 0;
            if (fPrefixPos < fPrefix.Length) {
                bytesFromPrefix = Math.Min(count, fPrefix.Length - fPrefixPos);
                Array.Copy(fPrefix, fPrefixPos, buffer, offset, bytesFromPrefix);
                fPrefixPos += bytesFromPrefix;
                offset += bytesFromPrefix;
                count -= bytesFromPrefix;
            }

            int bytesFromStream = count > 0 ? fStream.Read(buffer, offset, count) : 0;
            return bytesFromPrefix + bytesFromStream;
        }

        public override bool CanRead => fStream.CanRead;
        public override bool CanSeek => fStream.CanSeek;
        public override bool CanWrite => fStream.CanWrite;
        public override long Length => fStream.Length;

        public override long Position
        {
            get => fStream.Position;
            set => fStream.Position = value;
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            return fStream.Seek(offset, origin);
        }

        public override void SetLength(long value)
        {
            fStream.SetLength(value);
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            fStream.Write(buffer, offset, count);
        }
    }
}
