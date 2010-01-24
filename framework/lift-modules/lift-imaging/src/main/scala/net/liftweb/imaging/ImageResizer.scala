/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package imaging {


import java.awt.{Graphics2D, Graphics, Transparency, AlphaComposite, RenderingHints}
import java.awt.geom.AffineTransform 
import java.awt.image.{AffineTransformOp, BufferedImage, ColorModel, IndexColorModel}
import javax.imageio.ImageIO

import org.apache.sanselan.ImageReadException
import org.apache.sanselan.Sanselan
import org.apache.sanselan.ImageFormat
import org.apache.sanselan.common.IImageMetadata
import org.apache.sanselan.common.RationalNumber
import org.apache.sanselan.formats.jpeg.JpegImageMetadata
import org.apache.sanselan.formats.tiff.TiffField
import org.apache.sanselan.formats.tiff.TiffImageMetadata
import org.apache.sanselan.formats.tiff.constants.TagInfo
import org.apache.sanselan.formats.tiff.constants.TiffConstants
import org.apache.sanselan.formats.tiff.constants.TiffTagConstants

import java.io.{InputStream,ByteArrayOutputStream,ByteArrayInputStream}

import net.liftweb.util.IoHelpers

object ImageOutFormat extends Enumeration("png", "jpg"){
  val png,jpeg = Value
}

case class ImageWithMetaData(image:BufferedImage, orientation:Option[Int], format:ImageOutFormat.Value)

object ImageResizer {

  def getOrientation(imageBytes:Array[Byte]):Option[Int] = Sanselan.getMetadata(imageBytes) match {
    case metaJpg:JpegImageMetadata => 
      val exifValue = metaJpg.findEXIFValue(TiffTagConstants.TIFF_TAG_ORIENTATION)
      if (exifValue != null) Some(exifValue.getIntValue) else None
    case _ => None
  }
  
  def getImageFromStream(is:java.io.InputStream):ImageWithMetaData = {
    val imageBytes = IoHelpers.readWholeStream(is)
    val orientation = getOrientation(imageBytes)
    val format = Sanselan.guessFormat(imageBytes) match {
      case ImageFormat.IMAGE_FORMAT_JPEG => ImageOutFormat.jpeg
      case _ => ImageOutFormat.png
    }
    ImageWithMetaData(ImageIO.read(new java.io.ByteArrayInputStream(imageBytes)), orientation, format)
  }
  
  def imageToStream(format:ImageOutFormat.Value, image:BufferedImage):InputStream = {
    val outputStream = new ByteArrayOutputStream()
    ImageIO.write(image, format.toString, outputStream)
    new ByteArrayInputStream(outputStream.toByteArray)
  }
  
  /**
   * Resize to a square
   * Will preserve the aspect ratio of the original and than center crop the larger dimension.
   * A image of (200w,240h) squared to (100) will first resize to (100w,120h) and then take then crop
   * 10 pixels from the top and bottom of the image to produce (100w,100h)
   */
  def square(orientation:Option[Int], originalImage:BufferedImage, max:Int):BufferedImage = {
    val image = {
      val height = originalImage.getHeight
      val width = originalImage.getWidth
      val ratio:Double = width.doubleValue/height
      
      //set smaller dimension to the max
      val (scaledWidth, scaledHeight) = if (width < height) {
        (max,(max.doubleValue/ratio).intValue)
      } else {
        ((max.doubleValue*ratio).intValue, max)
      }
      resize(orientation, originalImage, scaledWidth, scaledHeight)
    }
    
    def halfDiff(dim:Int):Int = (dim-max)/2
    
    if (image.getHeight > max) {
      image.getSubimage(0,halfDiff(image.getHeight), image.getWidth, max)
    } else if (image.getWidth > max) {
      image.getSubimage(halfDiff(image.getWidth),0, max, image.getHeight)
    } else image
  }
  

  def scaledMaxDim(width:Int, height:Int , maxWidth:Int, maxHeight:Int):(Int,Int) = {
    val ratio:Double = width.doubleValue/height

    val scaleW = (maxWidth, (maxWidth.doubleValue/ratio).intValue)
    val scaleH = ((maxHeight.doubleValue*ratio).intValue,maxHeight)

    if (width > height && scaleW._2 <= maxHeight) 
      scaleW 
    else if (scaleH._1 <= maxWidth)
      scaleH
    else scaleW
  }
  
  /**
   * Resize to maximum dimension preserving the aspect ratio.  This is basically equivalent to what you would expect by setting
   * "max-width" and "max-height" CSS attributes but will scale up an image if necessary
   */
  def max(orientation:Option[Int],originalImage:BufferedImage, maxWidth:Int, maxHeight:Int):BufferedImage = {
    val (scaledWidth, scaledHeight) = scaledMaxDim(originalImage.getWidth, originalImage.getHeight, maxWidth, maxHeight)
    resize(orientation, originalImage, scaledWidth, scaledHeight)
  }
  
  /**
   * Algorithm adapted from example in Filthy Rich Clients http://filthyrichclients.org/
   * Resize an image and account of its orientation.  This will not preserve aspect ratio.
   */
  def resize(orientation:Option[Int], img:BufferedImage, targetWidth:Int, targetHeight:Int): BufferedImage = {
    val imgType = if (img.getTransparency() == Transparency.OPAQUE) BufferedImage.TYPE_INT_RGB else BufferedImage.TYPE_INT_ARGB
    var ret = img
    var scratchImage:BufferedImage = null
    var  g2:Graphics2D = null
    var w = img.getWidth
    var h = img.getHeight
    var prevW = ret.getWidth
    var prevH = ret.getHeight

    val isTranslucent:Boolean = img.getTransparency !=  Transparency.OPAQUE

    //If we're resizing down by more than a factor of two, resize in multiple steps to preserve image quality
    do {
      if (w > targetWidth) {
        w /= 2
        if (w < targetWidth) {
          w = targetWidth
        }
      } else w = targetWidth

      if (h > targetHeight) {
        h /= 2
        if (h < targetHeight) {
          h = targetHeight
        }
      } else h = targetHeight

      if (scratchImage == null || isTranslucent) {
        scratchImage = new BufferedImage(w, h, imgType);
        g2 = scratchImage.createGraphics
      }
      g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2.drawImage(ret, 0, 0, w, h, 0, 0, prevW, prevH, null)
      prevW = w
      prevH = h

      ret = scratchImage
    } while (w != targetWidth || h != targetHeight)

    if (g2 != null) {
      g2.dispose
    }

    // If we used a scratch buffer that is larger than our target size,
    // create an image of the right size and copy the results into it
    // If there is an orientation value other than the default, rotate the image appropriately
    if (targetWidth != ret.getWidth || targetHeight != ret.getHeight || orientation.map(_ != 1).getOrElse(false)) {

      val (tW, tH, rotFunc) =  orientation match {
        case Some(3) => // 3 => 180 (upside down)
          (targetWidth, targetHeight, (g2:Graphics2D) => {
            g2.rotate(Math.Pi)
            g2.translate(-targetWidth, -targetHeight)
          })
        case Some(6) => // 6 => -90 (counter clockwise)
          (targetHeight, targetWidth, (g2:Graphics2D) => {
            g2.rotate(Math.Pi/2)
            g2.translate(0, -targetHeight)
          })
        case Some(8) => // 8 => 90 (clockwise)
          (targetHeight, targetWidth, (g2:Graphics2D) => {
            g2.rotate(-Math.Pi/2)
            g2.translate(-targetWidth, 0)
          })
        case _ => (targetWidth, targetHeight, (g2:Graphics2D) => {})
      }
      scratchImage = new BufferedImage(tW, tH, imgType)
      g2 = scratchImage.createGraphics
      rotFunc(g2)
      g2.drawImage(ret, 0, 0, null)
      g2.dispose
      ret = scratchImage
    }

    ret
  }
     
} //ImageResizer
} //imaging
} //net.liftweb