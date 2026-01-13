/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Security.Cryptography;

namespace GKCore.Utilities
{
    internal class CRC64 : HashAlgorithm
    {
        private readonly ulong[] fTable;
        private ulong fHash;

        public CRC64()
        {
            HashSizeValue = 64;

            fTable = new ulong[256];
            for (int i = 0; i < 256; ++i) {
                ulong crc = (ulong)i;
                for (int j = 0; j < 8; ++j) {
                    if ((crc & 1) == 1)
                        crc = (crc >> 1) ^ 0xC96C5795D7870F42; // ECMA-182 polynomial
                    else
                        crc >>= 1;
                }
                fTable[i] = crc;
            }
        }

        public override void Initialize()
        {
            fHash = 0xFFFFFFFFFFFFFFFF;
        }

        protected override void HashCore(byte[] array, int ibStart, int cbSize)
        {
            for (int i = ibStart; i < ibStart + cbSize; i++) {
                var index = (byte)((fHash & 0xff) ^ array[i]);
                fHash = (fHash >> 8) ^ fTable[index];
            }
        }

        protected override byte[] HashFinal()
        {
            var result = BitConverter.GetBytes(~fHash);
            return result;
        }
    }

    public class ChecksumStream : Stream
    {
        private readonly HashAlgorithm fHashAlgorithm;
        private readonly Stream fTargetStream;

        public byte[] Checksum
        {
            get {
                fHashAlgorithm.TransformFinalBlock(new byte[0], 0, 0);
                return fHashAlgorithm.Hash;
            }
        }

        public ulong Checksum64
        {
            get {
                fHashAlgorithm.TransformFinalBlock(new byte[0], 0, 0);
                return BitConverter.ToUInt64(fHashAlgorithm.Hash, 0);
            }
        }

        public override bool CanRead
        {
            get { return true; }
        }

        public override bool CanSeek
        {
            get { return false; }
        }

        public override bool CanWrite
        {
            get { return true; }
        }

        public override long Length
        {
            get { return fTargetStream.Length; }
        }

        public override long Position
        {
            get { return fTargetStream.Position; }
            set { throw new NotSupportedException(); }
        }

        public ChecksumStream(Stream targetStream, HashAlgorithm hashAlgorithm = null)
        {
            fTargetStream = targetStream ?? throw new ArgumentNullException(nameof(targetStream));
            fHashAlgorithm = hashAlgorithm ?? new CRC64(); //SHA256.Create();
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            fHashAlgorithm.TransformBlock(buffer, offset, count, null, 0);
            fTargetStream.Write(buffer, offset, count);
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            var bytesRead = fTargetStream.Read(buffer, offset, count);
            if (bytesRead > 0) {
                fHashAlgorithm.TransformBlock(buffer, offset, bytesRead, null, 0);
            }
            return bytesRead;
        }

        public override void Flush()
        {
            fTargetStream.Flush();
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            throw new NotSupportedException();
        }

        public override void SetLength(long value)
        {
            fTargetStream.SetLength(value);
        }
    }
}
