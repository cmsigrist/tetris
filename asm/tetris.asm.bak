  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8


  ;; TODO Insert your code here

;;BEGIN:main
;;Test act: failed (test with a random state + the reset button) 1pt
;;Test draw_gsa: failed (draw_gsa some gsa drawn on the leds) 5pts (checked all the sp)
;;Test reset_game: failed (reset_game from a random state) 10pts (checked all the sp)
;;Test detect_full_line: failed (a gs with multiple full lines) 2pts (checked all the sp)
;;Test detect_full_line: failed (a gs with a full line) 2pts (checked all the sp)
main:
	addi sp, zero, 0x1FFC
	call reset_game

	outer_main_loop:
		inner_main_loop:
			addi t0, zero, 0
			inner_inner_main_loop:	
				addi sp, sp, -4
				stw t0, 0(sp)
				call draw_gsa
				call wait
				ldw t6, SCORE(zero)
				addi t7, zero, 9999
				beq t6, t7, main
				call display_score 	
			
				addi a0, zero, NOTHING
				call draw_tetromino			
				call get_input   
				beq v0, zero, no_input
				add a0, zero, v0
				call act
				no_input:
				addi a0, zero, FALLING
				call draw_tetromino	 
				call clear_leds
				call draw_gsa 
				call wait
				
				ldw t0, 0(sp)
				addi t0, t0, 1
				stw t0, 0(sp)
				addi sp, sp, 4
				cmplti t1, t0, RATE			;; test if less than RATE (then 1) ;;;;;;;;;;;;;;;;;;;;;CHANGED FOR THE TEST
				bne t1, zero, inner_inner_main_loop
		addi a0, zero, NOTHING
		call draw_tetromino
		addi a0, zero, moveD
		call act
		bne v0, zero, end_move_for_tetromino

		addi a0, zero, FALLING
		call draw_tetromino	
		call clear_leds
		call draw_gsa
		call wait
		jmpi inner_main_loop
		
		end_move_for_tetromino:		
		addi a0, zero, PLACED
		call draw_tetromino	
		
		while_full_line:
			call detect_full_line
			add a0, zero, v0
			cmpeqi t2, a0, Y_LIMIT
			bne t2, zero, end_detect
			call increment_score
			call remove_full_line
			call clear_leds
			call draw_gsa
			jmpi while_full_line
		end_detect:

		call generate_tetromino
		addi a0, zero, OVERLAP
		call detect_collision 
		cmpeqi t2, v0, NONE		;; no overlap (1)
		beq t2, zero, main 		;; if overlap
		addi a0, zero, PLACED
		call draw_tetromino
		call draw_gsa
		call wait
		jmpi outer_main_loop
ret
;;END:main

;; BEGIN:clear_leds
clear_leds:
	stw zero, LEDS(zero)
	stw zero, LEDS + 4(zero)
	stw zero, LEDS + 8(zero)
ret
;; END:clear_leds

;; BEGIN:set_pixel
set_pixel:	
	addi sp, sp, -4
	stw s2, 0(sp)										;;SAVED
	addi sp,sp,-12
	stw a0, 0(sp)
	stw a1, 4(sp)
	stw a2, 8(sp)

	addi t0, zero, 3	;; mask taking 2 lsbs
	and t0, a0, t0		;; x%4 
	slli t0, t0, 3		;; x%4 * 8
	add t0, t0, a1		;; x%4 * 8 + y
	addi t3, zero, 1	;; t3 <- 1
	sll t3, t3, t0		;; shift '1' de x%4 * 8 + y mask

	srli t1, a0, 2		;; x%4
	slli t1, t1, 2		;; x%4 * 4
	ldw t2, LEDS(t1)	;; load LED[x%4 * 4]
	or s2, t2, t3		;; LED[x%4 * 4] or t0
	
	stw s2, LEDS(t1)

	ldw a0, 0(sp)
	ldw a1, 4(sp)
	ldw a2, 8(sp)
	addi sp, sp, 12
	ldw s2, 0(sp)
	addi sp, sp, 4
 	
ret 
;; END:set_pixel

;; BEGIN:wait
wait:
	addi t0, zero, 1 		;; waiting time for silmulation set to 2 
	slli t0, t0, 19      	;; to the power 20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	decreasing_counter:
		addi t1, zero, 1		;; t1 contains 1
		sub t0, t0, t1			;; decrease t0 by 1
		bne t0, zero, decreasing_counter  	;; recall while t0 != 0
ret 
;;END:wait

;;BEGIN:in_gsa
in_gsa:
	cmplti t0, a0, 0	;; x < 0
	cmpgei t1, a0, 12	;; x >= 12
	or t2, t1, t0		;; x < 0 or x >= 12
	cmplti t0, a1, 0	;; y < 0
	cmpgei t1, a1, 8	;; y >= 8
	or t3, t0, t1		;; y < 0 or y >= 8
	or v0, t2, t3
ret
;;END:in_gsa

;;BEGIN:get_gsa
get_gsa:
	addi sp,sp,-4
	stw a0,0(sp)

	slli t0, a0, 3 	;; x * 8
	add t0, t0, a1  ;; x * 8 + y
	slli t0, t0, 2  ;; word aligned
	ldw v0, GSA(t0) 

	ldw a0, 0(sp)
	addi sp, sp, 4
ret
;;END:get_gsa

;;BEGIN:set_gsa
set_gsa:
	slli t0, a0, 3	;; same as get_gsa
	add t0, t0, a1	
	slli t0, t0, 2  ;; word aligned
	stw a2, GSA(t0)	;; store word p into GSA for word t0
ret
;;END:set_gsa

;;BEGIN:helper
get_offsets:
	ldw t0, T_X(zero) 			;; x anchor point
	ldw t1, T_Y(zero) 			;; y anchor point
	ldw t2, T_type(zero) 		;; type
	ldw t3, T_orientation(zero) ;; orientation
	
	slli t2, t2, 4 				;; 4 * types
	slli t3, t3, 2				;; 4 * orientation	
	add t3, t2, t3 				;; 4 * types + orientation
	ldw t4, DRAW_Ax(t3) 		;; x offset 
	ldw t5, DRAW_Ay(t3)			;; y offset
ret

x_offset:				;; takes a word counter as argument
	add t6, t3, t4 		;; selecting offset
	ldw t7, 0(t6)		;; loading offset
	add a0, t0, t7		;; adding to x its offset
ret

y_offset:				;; takes a word counter as argument
	add t6, t3, t5 		;; selecting offset		
	ldw t7, 0(t6)		;; loading offset
	add a1, t1, t7		;;adding to y its offset
ret

;;END:helper

;;BEGIN:draw_gsa
draw_gsa:
	addi sp, sp, -4
	stw ra, 0(sp)

	add a0, zero, zero
	add a1, zero, zero

	inner_:	
		addi sp, sp, -16
		stw a0, 0(sp)
		stw a1, 4(sp)
		stw t0, 8(sp)
		stw t1, 12(sp)
		call get_gsa 
		ldw a0, 0(sp)
		ldw a1, 4(sp)
		ldw t0, 8(sp)
		ldw t1, 12(sp)
		addi sp, sp, 16
			
		addi a2, v0, 0 			;; store the p value inside
		cmpeqi t1, a2, 1
		cmpeqi t2, a2, 2
		or t1, t1, t2
		cmpeqi t1, t1, 1 		;; 1 if p = 1 or p = 2
		addi t2, zero, 1 		;; constant 1
		
		bne t1, t2, next
		addi sp, sp, -20
		stw t0, 0(sp)
		stw t1, 4(sp)
		stw a0, 8(sp)	 ;;ADDED
		stw a1, 16(sp)	;;ADDED
		call set_pixel			;; set pixel if p >= 1
		ldw t0, 0(sp)
		ldw t1, 4(sp)
		ldw a0, 8(sp)	 ;;ADDED
		ldw a1, 16(sp)	;;ADDED
		addi sp, sp, 20

		next:
		addi a0, a0, 1			;; next x of the same line
		addi t0, zero, X_LIMIT

		bge a0, t0, outer_ 	;; if x > 12 then got to next line
		jmpi inner_				;; line is not finished

	outer_:
		addi a1, a1, 1			
		addi a0, zero, 0
		
		addi t0, zero, Y_LIMIT
		blt a1, t0, inner_ 

		ldw ra, 0(sp)
		addi sp, sp, 4		
ret
;;END:draw_gsa

;;BEGIN:draw_tetromino
draw_tetromino:
	addi sp, sp, -4
	stw ra, 0(sp)
	
	call get_offsets

	add a2, a0, zero			;; p
	add t3, zero, zero			;; word counter

	add a0, zero, t0
	add a1, zero, t1

	addi sp, sp, -8
	stw t0, 0(sp)
	stw t1, 4(sp)
	call set_gsa
	ldw t0, 0(sp)
	ldw t1, 4(sp)
	addi sp, sp, 8
		
	tetromino_loop: 
		addi sp, sp, -8
		stw t6, 4(sp)
		stw t7, 0(sp)
		call x_offset
		stw t6, 4(sp)
		stw t7, 0(sp)
		addi sp, sp, 8

		addi sp, sp, -8
		stw t6, 4(sp)
		stw t7, 0(sp)
		call y_offset
		stw t6, 4(sp)
		stw t7, 0(sp)
		addi sp, sp, 8
		
		addi sp, sp, -8
		stw t0, 0(sp)
		stw t1, 4(sp)
		call set_gsa	
		ldw t0, 0(sp)
		ldw t1, 4(sp)
		addi sp, sp, 8

		addi t3, t3, 4 		;; next offset
		addi t6, zero, 9
		blt t3, t6, tetromino_loop
	add a0, a2, zero 		;; replace old value (was changed to x value) to p
	ldw ra, 0(sp)
	addi sp, sp, 4	
ret 			
;;END:draw_tetromino

;;BEGIN:generate_tetromino
generate_tetromino:
	addi t0, zero, 0b111 		;; mask
	addi t1, zero, 5 

	shape:
	ldw t2, RANDOM_NUM(zero) 	;;generating random
	and t2, t2, t0				;;selecting the first 3 bits
	blt t2, zero ,shape
	bge t2, t1, shape
	
	addi t0, zero, START_X 		;;x
	addi t1, zero, START_Y 		;;y
	stw t0, T_X(zero)
	stw t1, T_Y(zero)

	add t0, zero, t2 			;;shape
	addi t1, zero, N 			;;orientation
	stw t0, T_type(zero)
	stw t1, T_orientation(zero)
	addi a0, zero, FALLING 	

ret
;;END:generate_tetromino

;;BEGIN:detect_collision
detect_collision:
	addi sp, sp, -16
	stw s0, 0(sp)
	stw s1, 4(sp)
	stw s2, 8(sp)
	stw ra, 12(sp)

	call get_offsets 			;; stores T_X into t0, T_Y into t1, x offset into t4 and y offset into t5

	addi t3, zero, -4			;; word counter

	add s0, zero, a0			;; save which collision is being checked

	addi s1, zero, W_COL
	beq s0, s1, W_collision	
	
	addi s1, zero, E_COL
	beq s0, s1, E_collision

	addi s1, zero, So_COL
	beq s0, s1, So_collision

	addi s1, zero, OVERLAP
	beq s0, s1, OLP
	
	W_collision:	
		addi s1, zero, -1  ;; move to the left
		addi s2, zero, 0
		add a0, t0, zero
		add a1, t1, zero
		jmpi c_loop

	E_collision:
		addi s1, zero, 1 ;; move to the right
		addi s2, zero, 0
		add a0, t0, zero
		add a1, t1, zero
		jmpi c_loop

	So_collision:
		addi s1, zero, 0
		addi s2, zero, 1 ;; move downwards
		add a0, t0, zero
		add a1, t1, zero
		jmpi c_loop

	OLP:
		addi s1, zero, 0 ;; checking overlap for current (has already moved)
		addi s2, zero, 0
		add a0, t0, zero
		add a1, t1, zero	

	c_loop:				
		add a0, a0, s1					;; next move
		add a1, a1, s2					;; next move

		addi sp, sp, -12
		stw t0, 0(sp)
		stw t1, 4(sp)
		stw t3, 8(sp)
		call in_gsa
		ldw t0, 0(sp)
		ldw t1, 4(sp)
		ldw t3, 8(sp)
		addi sp, sp, 12
		bne v0, zero, collision
		
		addi sp, sp, -8
		stw t0, 0(sp)
		stw t1, 4(sp)
		call get_gsa 
		ldw t0, 0(sp)
		ldw t1, 4(sp)
		addi sp, sp, 8	
		add t7, zero, v0 				;; load gsa of the pixel where the tetromino want to move
		cmpnei t7, t7, PLACED			;; no collison ? 1:0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; this should be the right way cmpeqi t7, t7, NOTHING  doesn't work
		beq t7, zero, collision			;; collision detected

		addi t3, t3, 4 					;; next offset

		addi sp, sp, -8
		stw t6, 0(sp)
		stw t7, 4(sp)
		call x_offset					;; stores in a0 x + offset  
		call y_offset					;; stores in a1 y + offset 
		ldw t6, 0(sp)
		ldw t7, 4(sp)
		addi sp, sp, 8

		cmplti t6, t3, 9				;; not the last
		bne t6, zero, c_loop 	
		jmpi end_collision_detection
			
collision:
	add a0, zero, s0
	add v0, zero, s0	;; else collision detected
	ldw s0, 0(sp)
	ldw s1, 4(sp)
	ldw s2, 8(sp)
	ldw ra, 12(sp)
	addi sp, sp, 16
ret

end_collision_detection:
	add a0, zero, s0
	addi v0, zero, NONE
	ldw s0, 0(sp)
	ldw s1, 4(sp)
	ldw s2, 8(sp)
	ldw ra, 12(sp)
	addi sp, sp, 16
ret

;;END:detect_collision

;;BEGIN: rotate_tetromino
rotate_tetromino:
	addi t0, zero, rotL
	addi t1, zero, rotR
	ldw t2, T_orientation(zero)
	beq a0, t0, rot_L
	
	rot_R:
	addi t2, t2, 1
	andi t2, t2, 0b11
	stw t2, T_orientation(zero)
ret
	rot_L:
	addi t2, t2, -1
	andi t2, t2, 0b11
	stw t2, T_orientation(zero)
ret
;;END: rotate_tetromino

;;BEGIN: act
act:
	addi v0, zero, 1 ;;move failes by default
	addi sp, sp, -12
	stw ra, 0(sp)
	stw a0, 4(sp)
	stw v0, 8(sp) 

	addi t0, zero, moveD	
	beq a0, t0, move_D
	addi t0, zero, moveL	
	beq a0, t0, move_L
	addi t0, zero, moveR	
	beq a0, t0, move_R
	addi t0, zero, rotL	
	beq a0, t0, rotation
	addi t0, zero, rotR	
	beq a0, t0, rotation
	addi t0, zero, reset
	beq a0, t0, reset_act

reset_act:
	stw zero, 8(sp)
	call reset_game
	jmpi end_act

move_D:
	addi a0, zero, So_COL
	call detect_collision
	addi t1, zero, NONE
	bne v0, t1, end_act		;;jumpi to end if there is a collision (unmodified)
	stw zero, 8(sp)			;; no collision so move is a success
	
	ldw t2, T_Y(zero)
	addi t2, t2, 1
	stw t2, T_Y(zero)
jmpi end_act

move_L:
	addi a0, zero, W_COL
	call detect_collision
	addi t1, zero, NONE
	bne v0, t1, end_act 
	stw zero, 8(sp)
	
	ldw t2, T_X(zero)
	addi t2, t2, -1
	stw t2, T_X(zero)
jmpi end_act	

move_R:
	addi a0, zero, E_COL
	call detect_collision
	addi t1, zero, NONE
	bne v0, t1, end_act 
	stw zero, 8(sp)
	
	ldw t2, T_X(zero)
	addi t2, t2, 1
	stw t2, T_X(zero)
jmpi end_act

rotation:
	;; t0 hasn't been changed t0 = a0 (move)
	addi t1, zero, 2 				;; counter (at most 2 moves)
	ldw t2, T_orientation(zero) 	;; original orientation
	ldw t3, T_X(zero)
	addi sp, sp, -16
	stw t0, 0(sp)
	stw t1, 4(sp)
	stw t2, 8(sp)
	stw t3, 12(sp)
	call rotate_tetromino ;; sets new T_orientation
	ldw t0, 0(sp)
	ldw t1, 4(sp)
	ldw t2, 8(sp)
	ldw t3, 12(sp)
	addi sp, sp, 16
	
	addi a0, zero, OVERLAP
	addi sp, sp, -20
	stw t0, 0(sp)
	stw t1, 4(sp)
	stw t2, 8(sp)
	stw t3, 12(sp)
	stw a0, 16(sp)
	call detect_collision
	ldw t0, 0(sp)
	ldw t1, 4(sp)
	ldw t2, 8(sp)
	ldw t3, 12(sp)
	ldw a0, 16(sp)
	addi sp, sp, 20	
	
	stw zero, 8(sp)		;; may change back to 1 if moved twice and there's still an overlap
	bne a0, v0, end_act ;; v0 != OVERLAP (= NONE) goto end
	add t5, zero, t3 	;; copy of T_X (s.t if move is not possible leave it unchanged 
	cmplti t4, t3, 6	;; Left side of the screen ? 1 : 0
	beq t4, zero, loop_right

loop_left:
	addi t4, zero, 1		;; move right
	jmpi try_loop

loop_right:	
	addi t4, zero, -1		;;move left
	jmpi try_loop

try_loop:
	add t5, t5, t4
	addi t1, t1, -1
	stw t5, T_X(zero)
	
	addi a0, zero, OVERLAP
	addi sp, sp, -24
	stw t0, 0(sp)
	stw t1, 4(sp)
	stw t2, 8(sp)
	stw t3, 12(sp)
	stw t4, 16(sp)
	stw t5, 20(sp)
	call detect_collision
	ldw t0, 0(sp)
	ldw t1, 4(sp)
	ldw t2, 8(sp)
	ldw t3, 12(sp)
	ldw t4, 16(sp)
	ldw t5, 20(sp)
	addi sp, sp, 24
	cmpne t6, t1, zero		;; while t1 != 0
	cmpnei v0, v0, NONE		;; collision? 1 : 0
	and t6, t6, v0 			;; less than 2 moves but there's still a collsion 1 : 0
	bne t6, zero, try_loop
	beq v0, zero, end_act

cannot_rotate:
	stw t2, T_orientation(zero)
	stw t3, T_X(zero)
	addi v0, zero, 1
	stw v0, 8(sp)
	jmpi end_act

end_act:
	ldw ra, 0(sp)
	ldw a0, 4(sp)
	ldw v0, 8(sp)
	addi sp, sp, 12	
ret
;;END:act

;;BEGIN:get_input
get_input:
	ldw t0, BUTTONS + 4(zero)
	addi t1, zero, 0b11111		;; mask
	and t0, t0, t1				;; input and mask
	addi t3, zero, 0 			;; shifter 

	beq t0, zero, result
	addi t3, zero, 1	
	loop:
		and t4, t0, t3	;; looks if the bit at the ith place is 1
		bne t4, zero, result
		slli t3, t3, 1			;; shift t3 of 1 to the left if not equal		
		jmpi loop
	
	result:
		stw zero, BUTTONS + 4(zero)
		add v0, t3, zero		
ret
;;END:get_input  

;;BEGIN:detect_full_line
detect_full_line:	
	addi sp, sp, -4
	stw ra, 0(sp)

	addi a0, zero, 0 		;; x loop
	addi a1, zero, 0 		;; y loop
	addi t0, zero, 0		;; counter
	addi t1, zero, 12		;; full line
	addi v0, zero, 8		;; default value in absence of a full line		

inner_X_loop:
	addi sp,sp, -16
	stw t0, 4(sp)
	stw t1, 0(sp)
	stw a0, 8(sp)
	stw a1, 12(sp)
	call get_gsa
	ldw t0, 4(sp)
	ldw t1, 0(sp)
	ldw a0, 8(sp)
	ldw a1, 12(sp)
	addi sp, sp, 16

	cmpeqi v0, v0, PLACED		;; if (x, y) is PLACED

	add t0, t0, v0			;; v0 is 1 if gsa = 1 so add 1 or 0
	beq t0, t1, end_full_line_detection ;; jump to end if a line is detected

	addi a0, a0, 1			;; next x of the same line

	addi sp,sp, -16
	stw t0, 4(sp)
	stw t1, 0(sp)
	stw a0, 8(sp)
	stw a1, 12(sp)
	call in_gsa
	ldw t0, 4(sp)
	ldw t1, 0(sp)
	ldw a0, 8(sp)
	stw a1, 12(sp)
	addi sp, sp, 16

	bne v0, zero, outer_Y_loop ;; if x > 12 then got to next line
	jmpi inner_X_loop		;; line is not finished

outer_Y_loop:
	addi a1, a1, 1			;; y will go up to 8 if no full line has been detect in the x loop
	addi a0, zero, 0
	
	addi sp,sp, -16
	stw t0, 4(sp)
	stw t1, 0(sp)
	stw a0, 8(sp)
	stw a1, 12(sp)
	call in_gsa
	ldw t0, 4(sp)
	ldw t1, 0(sp)
	ldw a0, 8(sp)
	ldw a1, 12(sp)
	addi sp, sp, 16

	addi t0, zero, 0

	beq v0, zero, inner_X_loop ;; while y is in gsa will loop over the x else if y = 8 -> got to end full line and store 8 in v0
	
end_full_line_detection:
	add v0, a1, zero
	ldw ra, 0(sp)
	addi sp, sp, 4
ret
;;END:detect_full_line

;;BEGIN:remove_full_line
remove_full_line:
	addi sp, sp, -4
	stw a0, 0(sp)
	add a1, a0, zero 	;; y value

blinking_lines:
	addi a2, zero, NOTHING 	;; PLACED(2) or NOTHING(0)
	addi sp, sp, -4
	stw ra, 0(sp)
	call lights	;; off
	ldw ra, 0(sp)
	addi sp, sp, 4

	addi a2, zero, PLACED
	addi sp, sp, -4
	stw ra, 0(sp)
	call lights	;; on
	ldw ra, 0(sp)
	addi sp, sp, 4

	addi a2, zero, NOTHING
	addi sp, sp, -4
	stw ra, 0(sp)
	call lights	;; off
	ldw ra, 0(sp)
	addi sp, sp, 4
	

	addi a2, zero, PLACED
	addi sp, sp, -4
	stw ra, 0(sp)
	call lights	;; on
	ldw ra, 0(sp)
	addi sp, sp, 4
	
	addi a2, zero, NOTHING
	addi sp, sp, -4
	stw ra, 0(sp)
	call lights	;; off
	ldw ra, 0(sp)
	addi sp, sp, 4

	jmpi moving_down

lights:
	addi a0, zero, 0
	col_loop:
		addi sp,sp, -20
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw a0, 8(sp)
		stw a1, 12(sp)
		stw a2, 16(sp)
		call set_gsa			;; set gsa with the given value
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw a0, 8(sp)
		ldw a1, 12(sp)
		ldw a2, 16(sp)
		addi sp, sp, 20

		addi a0, a0, 1
	
		addi sp,sp, -24
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		stw a0, 12(sp)
		stw a1, 16(sp)
		stw a2, 20(sp)
		call in_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		ldw a0, 12(sp)
		ldw a1, 16(sp)
		ldw a2, 20(sp)
		addi sp, sp, 24

		beq v0, zero, col_loop

		addi sp,sp, -4
		stw ra, 0(sp)
		call clear_leds				
		ldw ra, 0(sp)
		addi sp, sp, 4	
		
		addi sp,sp, -20
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw a0, 8(sp)
		stw a1, 12(sp)
		stw a2, 16(sp)
		call draw_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw a0, 8(sp)
		ldw a1, 12(sp)
		ldw a2, 16(sp)
		addi sp, sp, 20	
		
		addi sp,sp, -24
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		stw a0, 12(sp)
		stw a1, 16(sp)
		stw a2, 20(sp)
		call wait
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		ldw a0, 12(sp)
		ldw a1, 16(sp)
		ldw a2, 20(sp)
		addi sp, sp, 24
ret

moving_down:
		addi a0, zero, 0
		addi sp, sp, -8
		stw a1, 4(sp)		;; store at the bottom of the stack the value of removed line
		addi a1, a1, -1		
		stw a1, 0(sp)		;; store value of the line above
		
		addi sp,sp, -12
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		call in_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		addi sp, sp, 12

		bne v0, zero, end_remove 	;; line removed was line y = 0

	inner_loop:
		ldw a1, 0(sp)		;; line y - 1
		addi sp, sp, -8
		stw ra, 0(sp)
		stw t0, 4(sp)
		call get_gsa 		;; get the gsa of x and line y - 1
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		addi sp, sp, 8
		addi sp, sp, 4
		add a2, v0, zero 	;; store the value
			
		ldw a1, 0(sp) 		;; load inital value of y	
		addi sp, sp, -4		;; points back to value y - 1
		addi sp, sp, -8
		stw ra, 0(sp)
		stw t0, 4(sp)
		call set_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		addi sp, sp, 8
		
		addi a0, a0, 1			;; next x of the same line
	
		addi sp,sp, -12
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		call in_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		addi sp, sp, 12

		bne v0, zero, outer_loop ;; if x > 12 then got to next line
		jmpi inner_loop		;; line is not finished

	outer_loop:			;; for y from 0 to a1 move down
		ldw a1, 4(sp)
		addi a1, a1, -1 ;; original y - 1
		stw a1, 4(sp)	
		ldw a1, 0(sp) 
		addi a1, a1, -1			;; y will go up to 8 if no full line has been detect in the x loop
		stw a1, 0(sp)
		addi a0, zero, 0
	
		addi sp, sp, -12
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		call in_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		addi sp, sp, 12

		beq v0, zero, inner_loop
end_remove:
	addi a0, zero, 0
	addi a1, zero, 0
	addi a2, zero, NOTHING
	first_line_loop:
		addi sp,sp, -20
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw a0, 8(sp)
		stw a1, 12(sp)
		stw a2, 16(sp)
		call set_gsa			;; set gsa with the given value
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw a0, 8(sp)
		ldw a1, 12(sp)
		ldw a2, 16(sp)
		addi sp, sp, 20

		addi a0, a0, 1
	
		addi sp,sp, -24
		stw ra, 0(sp)
		stw t0, 4(sp)
		stw t1, 8(sp)
		stw a0, 12(sp)
		stw a1, 16(sp)
		stw a2, 20(sp)
		call in_gsa
		ldw ra, 0(sp)
		ldw t0, 4(sp)
		ldw t1, 8(sp)
		ldw a0, 12(sp)
		ldw a1, 16(sp)
		ldw a2, 20(sp)
		addi sp, sp, 24

		beq v0, zero, first_line_loop 	

		ldw a0, 0(sp)
		addi sp, sp, 4	
ret
;;END:remove_full_line

;;BEGIN: increment_score
increment_score:
	ldw t0, SCORE(zero)
	addi t0, t0, 1
	stw t0, SCORE(zero)

ret
;;END: increment_score

;;BEGIN:display_score
display_score:

	ldw t0, SCORE(zero)
	add t1, t0, zero 	;; save
	addi t2, zero, 12   ;; counter
	addi t7, t7, 0		;; counter division
	addi t3, zero, 0 	;; counter modulo

score_loop:
	add t0, zero, t1
	
modulo:
	cmpgeui t3, t1, 10 				;; t1 is remainder
	beq t3, zero, next_score
	addi t1, t1, -10
	bne t3, zero, modulo

next_score:
	slli t1, t1, 2					;; word alligned

	ldw t6, font_data(t1) 			;;get font
	stw t6, SEVEN_SEGS(t2) 			;; store in score
	beq t2,zero, end
	addi t2, t2, -4


	cmpltui t5, t0, 10
	addi t1, zero, 0
	bne zero, t5, score_loop 
	srli t1, t0, 1 					;; div by 2 original:t0
	add t3, zero, t1 				;; divided copy
	addi t1, zero, 0

	addi t5, zero, 0

	division: 						;; division by 10 
	addi t3, t3, -5
	addi t1, t1, 1 					;;count how many times t1 = remainder 
	cmpgeui t5, t3, 5
	bne zero, t5, division 

	jmpi score_loop
end:

ret
;;END: display_score

;;BEGIN: reset_game
reset_game: 
	addi sp, sp, -16
	stw a0, 0(sp)
	stw a1, 4(sp)
	stw a2, 8(sp)
	stw ra, 12(sp)

	stw zero, SCORE(zero)			
	call display_score
	
	addi a0, zero, 0 ;; x
	add t0, zero, a0
	addi a1, zero, 0 ;; y
	add t1, zero, a1
	addi a2, zero, 0 ;; p

  inner_reset:
  addi sp, sp, -20
  stw a0, 4(sp)
  stw a1, 8(sp)
  stw a2, 12(sp)
  stw t0, 16(sp)
  stw t1, 0(sp)
  call set_gsa
  ldw ra, 0(sp)
  ldw a0, 4(sp)
  ldw a1, 8(sp)
  ldw a2, 12(sp)
  ldw t0, 16(sp)
  ldw t1, 0(sp)
  addi sp, sp, 20

  addi a0, a0, 1			;; next x of the same line

  addi sp, sp, -20
  stw a0, 4(sp)
  stw a1, 8(sp)
  stw a2, 12(sp)
  stw t0, 16(sp)
  stw t1, 0(sp)
  call in_gsa
  ldw a0, 4(sp)
  ldw a1, 8(sp)
  ldw a2, 12(sp)
  ldw t0, 16(sp)
  ldw t1, 0(sp)
  addi sp, sp, 20
  bne v0, zero, outer_reset 	;; if x > 12 then got to next line
  jmpi inner_reset				;; line is not finished

outer_reset:
  addi a1, a1, 1			
  addi a0, zero, 0

  addi sp, sp, -20
  stw a0, 4(sp)
  stw a1, 8(sp)
  stw a2, 12(sp)
  stw t0, 16(sp)
  stw t1, 0(sp)
  call in_gsa
  ldw a0, 4(sp)
  ldw a1, 8(sp)
  ldw a2, 12(sp)
  ldw t0, 16(sp)
  ldw t1, 0(sp)
  addi sp, sp, 20

  beq v0, zero, inner_reset

  addi sp, sp, -8
  stw t0, 4(sp)
  stw t1, 0(sp) ;;ADDED
  call generate_tetromino
  ldw t0, 4(sp)
  ldw t1, 0(sp)
  addi sp, sp, 8


  addi sp, sp, -8
  stw t0, 4(sp)
  stw t1, 0(sp) ;;ADDED
  call draw_tetromino
  ldw t0, 4(sp)
  ldw t1, 0(sp)
  addi sp, sp, 8

  addi sp, sp, -8
  stw t0, 4(sp)
  stw t1, 0(sp) ;;ADDED
  call clear_leds
  ldw t0, 4(sp)
  ldw t1, 0(sp)
  addi sp, sp, 8

  
  addi sp, sp, -8
  stw t0, 4(sp)
  stw t1, 0(sp) ;;ADDED
  call draw_gsa
  ldw t0, 4(sp)
  ldw t1, 0(sp)
  addi sp, sp, 8


  addi sp, sp, -8
  stw t0, 4(sp)
  stw t1, 0(sp) ;;ADDED
  call wait
  ldw t0, 4(sp)
  ldw t1, 0(sp)
  addi sp, sp, 8

  ldw a0, 0(sp)
  ldw a1, 4(sp)
  ldw a2, 8(sp)
  ldw ra, 12(sp)
  addi sp, sp, 16

ret
;;END:reset_game

font_data:
    .word 0xFC  ; 0
    .word 0x60  ; 1
    .word 0xDA  ; 2
    .word 0xF2  ; 3
    .word 0x66  ; 4
    .word 0xB6  ; 5
    .word 0xBE  ; 6
    .word 0xE0  ; 7
    .word 0xFE  ; 8
    .word 0xF6  ; 9

C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
    .word C_N_X
    .word C_E_X
    .word C_So_X
    .word C_W_X
    .word B_N_X
    .word B_E_X
    .word B_So_X
    .word B_W_X
    .word T_N_X
    .word T_E_X
    .word T_So_X
    .word T_W_X
    .word S_N_X
    .word S_E_X
    .word S_So_X
    .word S_W_X
    .word L_N_X
    .word L_E_X
    .word L_So_X
    .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
    .word C_N_Y
    .word C_E_Y
    .word C_So_Y
    .word C_W_Y
    .word B_N_Y
    .word B_E_Y
    .word B_So_Y
    .word B_W_Y
    .word T_N_Y
    .word T_E_Y
    .word T_So_Y
    .word T_W_Y
    .word S_N_Y
    .word S_E_Y
    .word S_So_Y
    .word S_W_Y
    .word L_N_Y
    .word L_E_Y
    .word L_So_Y
    .word L_W_Y