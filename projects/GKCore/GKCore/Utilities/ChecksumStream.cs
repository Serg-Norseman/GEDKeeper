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

using System;
using System.IO;
using System.Security.Cryptography;

namespace GKCore.Utilities
{
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
            fHashAlgorithm = hashAlgorithm ?? SHA256.Create();
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
