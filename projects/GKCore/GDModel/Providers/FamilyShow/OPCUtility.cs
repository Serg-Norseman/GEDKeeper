// This file is part of "Family.Show".

using System;
using System.IO;
using System.IO.Packaging;

namespace GDModel.Providers.FamilyShow
{
    /// <summary>
    /// Utility class for packaging and reading Open Package Convention files.
    /// NOTE: This class is based on the PackageWrite and PackageRead samples in the Microsoft Windows SDK.
    /// It has been extended to work with directories and additional file formats such as jpegs and rtf.
    /// </summary>
    public static class OPCUtility
    {
        private const string PackageRelationshipType =
            @"http://schemas.microsoft.com/opc/2006/sample/document";

        private const string ResourceRelationshipType =
            @"http://schemas.microsoft.com/opc/2006/sample/required-resource";

        /// <summary>
        ///   Extracts content and resource parts from a given Package
        ///   zip file to a specified target directory.</summary>
        /// <param name="packagePath">
        ///   The relative path and filename of the Package zip file.</param>
        /// <param name="targetDirectory">
        ///   The relative path from the current directory to the targer folder.
        /// </param>
        public static void ExtractPackage(string packagePath, string targetDirectory)
        {
            // If we are opening a new file, clear the current files temporary extracted files.
            try {
                // Create a new Target directory.  If the Target directory
                // exists, first delete it and then create a new empty one.
                DirectoryInfo directoryInfo = new DirectoryInfo(targetDirectory);
                if (directoryInfo.Exists)
                    directoryInfo.Delete(true);
                directoryInfo.Create();
            } catch {
                // ignore errors
            }

            // Open the Package.
            using (Package package = Package.Open(packagePath, FileMode.Open, FileAccess.Read)) {
                PackagePart documentPart = null;

                // Get the Package Relationships and look for the Document part based on the RelationshipType
                Uri uriDocumentTarget = null;
                foreach (PackageRelationship relationship in package.GetRelationshipsByType(PackageRelationshipType)) {
                    // Resolve the Relationship Target Uri so the Document Part can be retrieved.
                    uriDocumentTarget = PackUriHelper.ResolvePartUri(new Uri("/", UriKind.Relative), relationship.TargetUri);

                    // Open the Document Part, write the contents to a file.
                    documentPart = package.GetPart(uriDocumentTarget);
                    ExtractPart(documentPart, targetDirectory);
                }

                // Get the Document part's Relationships, and look for required resources.
                Uri uriResourceTarget = null;
                foreach (PackageRelationship relationship in documentPart.GetRelationshipsByType(ResourceRelationshipType)) {
                    // Resolve the Relationship Target Uri so the Resource Part can be retrieved.
                    uriResourceTarget = PackUriHelper.ResolvePartUri(documentPart.Uri, relationship.TargetUri);

                    // Open the Resource Part and write the contents to a file.
                    PackagePart resourcePart = package.GetPart(uriResourceTarget);
                    ExtractPart(resourcePart, targetDirectory);
                }
            }
        }

        /// <summary>
        ///   Extracts a specified package part to a target folder.  Does not overwrite existing histories, photos and attachments</summary>
        /// <param name="packagePart">
        ///   The package part to extract.</param>
        /// <param name="targetDirectory">
        ///   The absolute path to the targer folder.</param>
        private static void ExtractPart(PackagePart packagePart, string targetDirectory)
        {
            try {
                // Create a string with the full path to the target directory.
                string pathToTarget = targetDirectory;

                // Remove leading slash from the Part Uri, and make a new Uri from the result
                string stringPart = packagePart.Uri.ToString().TrimStart('/');
                Uri partUri = new Uri(stringPart, UriKind.Relative);

                // Create a full Uri to the Part based on the Package Uri
                Uri uriFullPartPath = new Uri(new Uri(pathToTarget, UriKind.Absolute), partUri);

                // Create the necessary Directories based on the Full Part Path
                Directory.CreateDirectory(Path.GetDirectoryName(uriFullPartPath.LocalPath));

                // Create the file with the Part content
                using (FileStream fileStream = new FileStream(uriFullPartPath.LocalPath, FileMode.Create)) {
                    CopyStream(packagePart.GetStream(), fileStream);
                }
            } catch {
                // ignore errors
            }
        }

        /// <summary>
        /// Copies data from a source stream to a target stream.
        /// NOTE: This method was taken from the PackageWrite sample in the Microsoft Windows SDK
        /// </summary>
        public static void CopyStream(Stream source, Stream target)
        {
            const int bufSize = 0x1000;
            byte[] buf = new byte[bufSize];
            int bytesRead = 0;

            while ((bytesRead = source.Read(buf, 0, bufSize)) > 0)
                target.Write(buf, 0, bytesRead);
        }
    }
}
