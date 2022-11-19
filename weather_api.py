#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 14 13:49:06 2021

@author: myrandaswartzwelter
"""


#DSC 510 -T301 Introduction to Programming
#Week 12: Final Project
#Final Project: Weather Program
#Author: Myranda Swartzwelter
#Date: 11/14/2021

import requests
import sys

def yes_no(user_input): #there are several instances where we ask the user for a yes or no input. This is used for those situations
    if user_input.lower() == 'yes' or user_input.lower() == 'y': #valid inputs for yes are: Y y yes YES Yes YeS yEs yeS
            response = user_input_location() #ask the user whether they'd like to look up weather
    else: #If not a valid yes option, give exit message and exit
            print('Thanks! Have a great day and enjoy the weather!')
            sys.exit(0) 

def user_input_location(): #asks user if they would like to look up location by city or zipcode and starts workflow to get weather
        location_type = input('Would you like to look up weather by US City or Zipcode? Please enter "City" for US City or "Zipcode" for US Zipcode.')
        response = determine_user_params(location_type) #determines answer and gets response
        response_values(response) #calls function to get values and print
        return 

def check_api_request(response): #check whether the response from the api request is valid
    main = response.get('main')
    try:
        main_weather = main.get('main')
    except:
        message = response.get('message') #if there's an error with getting weather, tell user what the error is
        user_input = input(f'Oh no, it looks like there is an error! \nThe message from the system: is {message}. \nWould you like to try again?')
        yes_no(user_input)
    return 
    
def city_state_api_request(city, state, units): #function to call api for city
    response = requests.get(f'https://api.openweathermap.org/data/2.5/weather?q={city}&units={units}&appid=96dc48c8efc3152d23f8a3d23d4bf548')
    response = response.json()
    check_api_request(response) #check that api response contains required elements
    return response

def zipcode_api_request(zipcode, units): #function to call api for zipcode
    response = requests.get(f'https://api.openweathermap.org/data/2.5/weather?zip={zipcode}&units={units}&appid=96dc48c8efc3152d23f8a3d23d4bf548')
    response = response.json()
    check_api_request(response) #check whether api response contains required elements
    return response

def response_values(response): #parses out response values from json blob returned by api
    weather  = response.get('weather')
    main = response.get('main')
    clouds = response.get('clouds')
    description = main.get('description')
    current_temp = main.get('temp')
    high_temp = main.get('temp_max')
    low_temp = main.get('temp_min')
    pressure = main.get('pressure')
    humidity = main.get('humidity')
    clouds = clouds.get('all')
    city = response.get('name')
    main_description = weather[0].get('main') #weather key values are wrapped in an array (array of dictionaries)
    description = weather[0].get('description')
    format_response(city, current_temp, low_temp, high_temp, pressure,humidity,clouds,main_description, description)
    return 


def format_response(city, current_temp, low_temp, high_temp, pressure,humidity,clouds,main_description, description): #pretty print the weather
    print('')
    print('+-+-+-+-+-+-+-+-+-+-+-+-+-+-+')
    print(f'Current weather conditions for {city}:')
    print(f'Weather: {main_description} with {description}')
    print(f'Current temp: {current_temp} degrees.')
    print(f'Low temp: {low_temp} degrees.')
    print(f'High temp: {high_temp} degrees.')
    print(f'Pressure: {pressure}hPa')
    print(f'Humidity: {humidity}%')
    if float(clouds) > 75: #cloud value given is not helpful. Create case statement for values
        clouds = 'Full Cloud Cover'
    elif float(clouds) > 50:
        clouds = 'Mostly Cloudy'
    elif float(clouds) > 25:
        clouds = 'Low cloud cover'
    else:
        clouds = 'No clouds'
    print(f'Cloud cover: {clouds}.')
    print('+-+-+-+-+-+-+-+-+-+-+-+-+-+-+')
    
    user_input = input('Would you like to find the weather for a different location? Yes or No')
    yes_no(user_input)
    return

def unit_check(units): #translate units from user to units used by API. Default to farenheit 
    if units.lower() == 'c': #celsius
        units = 'metric'
    elif units.lower() == 'k': #kelvin
        units = 'standard'
    else:
        units = 'imperial' #default to imperial
    return units

def determine_user_params(location_type): #determines based on user input whether the city api url or zipcode api curl should be called
    if location_type.lower() == 'city' or location_type.lower() == 'c':
        city = input('Please enter the US City.') #get city
        state = input('Please enter the US State.') #get state
        units = input('Would you like to view the temperatures in Fahrenheit, Celsius, or Kelvin? Please enter "F" for  Fahrenheit, "C" for Celsius or "K" for Kelvin.')
        units = unit_check(units) #convert user units to api units
        response = city_state_api_request(city,state, units) #call api
    elif location_type.lower() == 'zipcode' or location_type.lower() == 'z':
        zipcode = input('Please enter the US Zipcode') #get zipcode
        units = input('Would you like to view the temperatures in Fahrenheit, Celsius, or Kelvin? Please enter "F" for  Fahrenheit, "C" for Celsius or "K" for Kelvin.')
        units = unit_check(units) #convert user units to api units
        response = zipcode_api_request(zipcode, units) #call api
    else:
        try_again = input('It looks like you did not answer with Zipcode or City, would you like to try again? Yes or No?') #if not city or zipcode, give user another chance
        yes_no(try_again)                         
    return response



def main():
    string = int(9.9)
    print(string)

#    user_input_location() #start program
    



if __name__ == "__main__":            
    main()   
        