#include <Encoder.h>
#include <Keyboard.h>
#include <Mouse.h>
Encoder dial(4, 5);

//#define Key_Volume_Up 0xB0
#define Key_Volume_Up 24
//0xAF
#define Key_Volume_Down 0xEA

long offset = 2000 * 4;
long mode = 0;
long pos = offset;
long lastButtonPress = 0;
long debounceDelay = 50;  

// the setup function runs once when you press reset or power the board
void setup() {
  // initialize digital pin LED_BUILTIN as an output.
  for( int i = 0; i<12; i++) {
    pinMode(6+i, OUTPUT);
  }
  pinMode(3, INPUT_PULLUP);
  Keyboard.begin();
  playAnim();
  pos = (dial.read() + offset)/4;
  setLed(mode);
}

void setLed(int n){
  for( int i = 0; i<12; i++) {
    digitalWrite(6+i, i == (11-n) ? HIGH : LOW); 
  }
}

void playAnim(){
  for(int j = 0; j<3; j++){
    for( int i = 0; i<12; i++) {
      setLed(i); 
      delay(25);
    }
    for( int i = 10; i>0; i--) {
      setLed(i); 
      delay(20);
    }
  }         
}

void doKeypress(long dir, long down, long up) {
  Keyboard.press(dir < 0 ? down : up);
}

void doAction (long dir){
  switch( mode) {
     case 0:
         doKeypress(dir, KEY_MEDIA_VOLUME_INC, KEY_MEDIA_VOLUME_DEC);
         break;
     case 1:
         Mouse.move(0, 0, dir);
         break;
     case 2:
         Mouse.move(-dir*25, 0, 0);
         break;
     case 3:
         Mouse.move(0, -dir*25, 0);
         break;
     case 4:
         Mouse.press(dir<0? MOUSE_RIGHT : MOUSE_LEFT);
         Mouse.release(dir<0? MOUSE_RIGHT : MOUSE_LEFT);
         break;
     case 5:
         doKeypress(dir, KEY_RIGHT, KEY_LEFT);
         break;
     case 6:
         doKeypress(dir, KEY_DOWN, KEY_UP);
         break;
     case 7:
         doKeypress(dir, KEY_PAGE_DOWN, KEY_PAGE_UP);
         break;
    case 8:
         Keyboard.press(MODIFIERKEY_ALT);
         if(dir > 0){
          Keyboard.press(MODIFIERKEY_SHIFT);
         }
         Keyboard.press(KEY_TAB);
         break;
    case 9:
         Keyboard.press(MODIFIERKEY_CTRL);
         if(dir > 0){
           Keyboard.press(MODIFIERKEY_SHIFT);
         }
         Keyboard.press(KEY_TAB);
         break;
    case 10:
         if(dir > 0){
           Keyboard.press(MODIFIERKEY_SHIFT);
         }
         Keyboard.press(KEY_N);
         break;
    case 11:
         doKeypress(dir, KEY_MEDIA_NEXT_TRACK, KEY_MEDIA_PREV_TRACK);
         break;
  }

  Keyboard.releaseAll();
  
}

void loop() {
  long newPos;
  long tNew = millis();
  newPos = (dial.read() + offset)/4;
  if (newPos != pos && tNew > lastButtonPress + debounceDelay) {
    bool isDown = !digitalRead(3);
    long dir = newPos - pos;
    if(isDown){
      mode = (12 + mode - dir) % 12;
      setLed(mode);
    } else {
      doAction(dir);
    }
    lastButtonPress = tNew;
    pos = newPos;
  }
  
}
