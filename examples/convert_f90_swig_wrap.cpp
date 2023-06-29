#include <iostream>
#include <string>

void convert_line(std::string src, std::string & dst);

int main(int argc, char**argv) {
    std::string line;
    std::string newline;
    while (!std::cin.eof()) {
      std::getline (std::cin,line);
      convert_line(line,newline);
      std::cout << newline << std::endl;  
    }
}

void convert_line(std::string src, std::string & dst) {
    size_t ii;
    int found_wrap;
    found_wrap=0;
    ii = 0;
    dst.clear();
    while (ii < src.length()) {
      switch (found_wrap) {
        case 0:
          if (src[ii] !='_') {
            dst.push_back(src[ii]);
            ii++;
            continue;
          } else {
            if (ii+5 < src.length()
              && src[ii+1] == 'w' 
              && src[ii+2] == 'r'
              && src[ii+3] == 'a'
              && src[ii+4] == 'p'
              && src[ii+5] == '_') {
              ii+= 6;
              found_wrap=1;
              dst.push_back('s');
              dst.push_back('w');
              dst.push_back('i');
              dst.push_back('g');
              dst.push_back('c');
              dst.push_back('_');
              continue;
            } else {
              dst.push_back(src[ii]);
              ii++;
              continue;
            }
          }
          break;
        case 1:
          if (src[ii] == ' '
            || src[ii] == '\"'
            || src[ii] == '(' ) {
              dst.push_back('_');
              dst.push_back(src[ii]);
              found_wrap=0;
              ii++;
              continue;
          } else {
            dst.push_back(src[ii]);
            ii++;
            continue;
          }
       break;
      }
    }
}
