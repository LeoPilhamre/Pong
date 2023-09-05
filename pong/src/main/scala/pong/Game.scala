package pong

import panda.Engine
import pong.utils.* 

import javax.swing.{JPanel}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Graphics
import java.awt.Point
import scala.collection.mutable.{Map, ListBuffer}
import scala.languageFeature.postfixOps
import java.awt.Font


class Game extends Engine
{
	val title: String = "PONG"

	val windowSize: Dimension = new Dimension(1000, 700)

	val framerate: Int = 60
	var gamePanel: JPanel = null

	var isPlaying: Boolean = false

	val colors: Map[String, Color] = Map(
		"playing-bg"	-> Color.darkGray,
		"bg"			-> Color.darkGray,
		"start-text"	-> Color.white,
		"paddle"		-> Color.white,
		"ball"			-> Color.white,
		"score"			-> Color.white
	)


	sealed trait Direction(val v: Int)
	object Direction
	{
		case object Up extends Direction(1)
		case object Down extends Direction(-1)
		case object Static extends Direction(0)
	}

	trait Paddle
	{
		var position: Vec2 = null

		val size: Dimension = new Dimension(20, 100)

		val speed: Double = 0.5d // units / seconds

		var direction: Direction = Direction.Static

		def move(delta: Double, dir: Direction = direction): Unit =
		{
			direction = dir

			if dir == Direction.Static then return // only for speed

			val t: Double = System.currentTimeMillis()
			position.y += speed * delta * -dir.v
			position.y = math.max(0, math.min(windowSize.height - size.height + offsetFix.y, position.y))
		}
	}

	class Player(val isRight: Boolean) extends Paddle
	{
		var score: Int = 0

		private val y = windowSize.height / 2 - size.height / 2
		private val k = windowSize.width / 10 - size.width / 2
		if (isRight)
			position = Vec2(windowSize.width - k, y)
		else
			position = Vec2(k, y)
	}

	val player1: Player = new Player(false)
	val player2: Player = new Player(true)

	class Ball
	{
		val size: Dimension = new Dimension(10, 10)
		
		val startingSpeed: Double = 0.15d
		var speed: Double = startingSpeed
		val acceleration: Double = 0.025d

		def getStartVel: Vec2 =
			return Vec2(if math.random() > 0.5d then 1.0d else -1.0d, math.random() - 0.5d)

		var velocity = getStartVel

		val startingPos: Vec2 = Vec2(
			windowSize.width / 2 - size.width / 2,
			windowSize.height / 2 - size.height / 2)
		var position: Vec2 = startingPos.copy()

		def move(delta: Double): Unit = 
		{
			speed += acceleration / 1000d * delta

			position.x += velocity.x * speed * delta
			position.y += velocity.y * speed * delta
		}

		def collide(players: Player*): Unit =
		{
			for (player <- players)
			{
				if (isOverlap(position, size, player.position, player.size))
					velocity = calculateReflection(if player.isRight then Vec2(-1,0) else Vec2(1, 0))
			}
			if (position.y <= 0)
				velocity = calculateReflection(Vec2(0,-1))
			if (position.y >= windowSize.height + offsetFix.y)
				velocity = calculateReflection(Vec2(0,1))

			if (position.x <= 0)
				win(player2)
			else if (position.x >= windowSize.width + offsetFix.x)
				win(player1)
		}

		private def win(player: Player): Unit =
		{
			position = startingPos.copy()
			velocity = getStartVel
			speed = startingSpeed
			player.score += 1
		}
			
		private def calculateReflection(n: Vec2): Vec2 = 
		{
			// Calculate the new velocity using the reflection formula
			return velocity - n * (2 * (velocity * n))
		}

		private def isOverlap(p1: Vec2, s1: Dimension, p2: Vec2, s2: Dimension): Boolean =
		{
			val l1: Vec2 = p1
			val r1: Vec2 = Vec2(p1.x + s1.width, p1.y + s1.height)
			val l2: Vec2 = p2
			val r2: Vec2 = Vec2(p2.x + s2.width, p2.y + s2.height)
			
			// Check if one rectangle is to the left of the other
			if (r1.x < l2.x || r2.x < l1.x)
				return false
			else if (r1.y < l2.y || r2.y < l1.y)
				return false
			else
				return true
		}
	}

	val ball: Ball = new Ball


	override def start: Unit = 
	{
		gamePanel = new GamePanel

		super.start
	}

	override def run(delta: Double): Unit =
	{
		super.run(delta)

		if (isPlaying)
		{
			player1.move(delta)
			player2.move(delta)

			ball.move(delta)

			ball.collide(player1, player2)
		}
	}


	override def keyDown(key: Int): Unit = 
	{
		super.keyDown(key)

		key match
			case 'P' => isPlaying = !isPlaying
			case 'W' => player1.direction = Direction.Up
			case 'S' => player1.direction = Direction.Down
			case 'O' => player2.direction = Direction.Up
			case 'L' => player2.direction = Direction.Down
			case default => ;
	}

	def keyUp(key: Int): Unit = 
	{
		key match
			case 'W'| 'S' => player1.direction = Direction.Static
			case 'O' | 'L' => player2.direction = Direction.Static
			case default => ;
	}

	def keyKB(key: Int): Unit =
	{
		
	}

	def onClick(x: Int, y: Int): Unit =
	{
		
	}


	class GamePanel extends JPanel
	{
		override def paint(g: Graphics): Unit =
		{
			super.paint(g)
			
			if (isPlaying)
				g.setColor(colors("playing-bg"))
				g.fillRect(0, 0, frame.getWidth(), frame.getHeight())

				g.setColor(colors("paddle"))
				drawPlayer(g, player1)
				drawPlayer(g, player2)

				drawBall(g)

				drawScore(g)
			else
			{
				g.setColor(colors("bg"))
				g.fillRect(0, 0, frame.getWidth(), frame.getHeight())

				drawStartText(g)
			}
		}

		def drawStartText(g: Graphics) =
		{
			g.setColor(colors("start-text"))
			val font: Font = new Font("Comic Sans MS", Font.BOLD, 15);
			g.setFont(font)
			g.drawString("PRESS P TO START", windowSize.width / 2 - 100, windowSize.height / 2 - 10)
		}

		def drawPlayer(g: Graphics, plr: Player): Unit =
		{
			g.fillRect(plr.position.x.toInt, plr.position.y.toInt, plr.size.width, plr.size.height)
		}

		def drawBall(g: Graphics): Unit =
		{
			g.setColor(colors("ball"))
			g.fillOval(ball.position.x.toInt, ball.position.y.toInt, ball.size.width, ball.size.height)
		}

		def drawScore(g: Graphics): Unit =
		{
			g.setColor(colors("score"))
			val font: Font = new Font("Comic Sans MS", Font.BOLD, 60);
			g.setFont(font)
			g.drawString(s"${player1.score.toString()} : ${player2.score.toString()}", windowSize.width / 2 - 75, 100)
		}
	}
}